# Error Handling

Error handling is important, but if it obscures logic, it's wrong.

## Use Exceptions Rather Than Return Codes

Back in the past there were many languages that didn't have exceptions. You either set an error flag or returned an error code that the caller could check.

```DeviceController.java```:

```java
public class DeviceController {
    ...
    public void sendShutDown() {
        DeviceHandle handle = getHandle(DEV1);
        // Check the state of the device
        if(handle != DeviceHandle.INVALID) {
            // Save the device status to the record field
            retrieveDeviceRecord(handle);
            //If not suspended, shut down
            if(record.getStatus() != DEVICE_SUSPENDED) {
                pauseDevice(handle);
                clearDeviceWorkQueue(handle);
                closeDevice(handle);
            } else {
                logger.log("Device suspended. Unable to shut down");
            }
        } else {
            logger.log("Invalid handle for: " + DEV1.toString());
        }
    }
}
```

The problem with these approaches is that they clutter the caller. The caller must check for errors immediately after the call. Unfortunately, it is easy to forget. For this reason it is better to throw an exception when you encounter an error. The calling code is cleaner. Its logic is not obscured by error handling.

```java
public class DeviceController {
    try {
        tryToShutDown();
    } catch (DeviceShutDownError e) {
        logger.log(e);
    }
}

private void tryToShutDown() throws DeviceShutDownError {
    DeviceHadnle handle = getHandle(DEV1);
    DeviceRecord record = retrieveDeviceRecord(handle);

    pauseDevice(handle);
    clearDeviceWorkQueue(handle);
    closeDevice(handle);
}

private DeviceHandle getHandle(DeviceID id) {
    ...
    throw new DeviceShutDownError("Invalid handle for: " + id.toString());
    ...
}
```

The algorithm for device shutdown and error handling are now separated. You can look at each of those concerns and understand them independently.

## Write Your ```Try-Catch-Finally``` Statement First

One of the most interesting things about exceptions is that they _define a scope_ within your program. When you execute code in the ```try``` portion of a ```try-catch-finally``` statement, you are stating that execution can abort at any portion an then resume at the ```catch```.

In a way, ```try``` blocks are like transactions. Your ```catch``` has to leave your program in a consisten state, no matter what happens in the ```try```. For this reason it is good practice to start with a ```try-catch-finally``` statement when you are writing code that could throw exceptions.

### Example

We need to write some code that accesses a file and reads some serialized objects.

We start with a unit test that shows that we'll get an exception when the file doesn't exist:

```java
@Test(expected = StorageException.class)
public void retrieveSectionShouldThrowOnInvalidFileName() {
    sectionStore.retrieveSection("invalid - file");
}
```

The test drives us to create this stub:

```java
public List<RecordedGrip> retrieveSection(String sectionName) {
    // dummy return until we have a real implementation
    return new ArrayList<RecordedGrip>();
}
```

Our test fails because it doesn't throw an exception. Next, we change our implementation so that it attempts to access an invalid file. This operation throws an exception:

```java
public List<RecordedGrip> retrieveSection(String sectionName) {
    try {
        FileInputStream stream = new FileInputStream(sectionName);
    } catch (Exception e) {
        throw new StorageException("retrieval error", e);
    }
    return new ArrayList<RecordedGrip>();
}
```

Our test passes now because we've caught the exception. At this point, we can refactor. We can narrow the type of the exception we catch to match the type that is actually thrown from the ```FileInputStream``` constructor: ```FileNotFoundException```.

```java
public List<RecordedGrip> retrieveSection(String sectionName) {
    try {
        FileInputStream stream = new FileInputStream(sectionName);
        stream.close();
    } catch (Exception e) {
        throw new StorageException("retrieval error", e);
    }
    return new ArrayList<RecordedGrip>();
}
```

Now that we've defined the scope with a ```try-catch``` structure, we can use TDD to build up the rest of the logic that we need. That logic will be added between the creation of the ```FileInputStream``` and the ```close```, and can pretend that nothing goes wrong.

**Try to write tests that force exceptions**, and then add behavior to your handler to satisfy your tests.

## Use Unchecked Exceptions

The _price_ of checked exceptions is an Open/Closed Principle violation. If you throw a checked exception from a method in your code and the ```catch``` is three levels above, you must declare that exception in the signature of each method between you and the ```catch```. This means that a change at a low level of the software can force signature changes on many higher levels. The changes modules must be rebuilt and redeployed, even though  nothing they care about changed.

Encapsulation is broken because all functions in the path of a throw must know about details of that low-level exception.

Checked exceptions can sometimes be useful if you are writing a critical library. You must catch them. But in general application development the dependency costs outweigh the benefits.

## Provide Context with Exceptions

Each exception that you throw should provide enough context to determine the source and location of an error.

Create informative error messages and pass them along with your exceptions. Mention the operation that failed and the type of failure. If you are logging in your application, pass along enough information to be able to log the error in your ```catch```.

## Define Exception Classes in Terms of a Caller's Needs

There are many ways to classify errors. We can classify them by their source: *Did they come from one component or another?* On their type: *Are they device failures, network failures, or programming errors?* However, when we define exception classes in an application, our most important concern should be how they are caught.

### The ```ACMEPort``` example

Here is a ```try-catch-finally``` statement for a third-party library call. It covers all of the exceptions that the library could throw:

```java
ACMEPort port = new ACMEPort(12);

try {
    port.open();
} catch (DeviceResponseException e) {
    reportPortError(e);
    logger.log("Device Response Exception", e);
} catch (ATM1212UnlockedException e) {
    reportPortError(e);
    logger.log("Unlock exception", e);
} catch (GMXError e) {
    reportPortError(e);
    logger.log("Device Response Exception");
} finally {
    ...
}
```

The statement contains a lot of duplication. In most exception handling situations, the work that we do is relatively standard regardless of the actual cause. We have to record an error and make sure that we can proceed.

We can simplify our code considerably by wrapping the API that we are calling and making sure that it returns a common exception type.

```java
LocalPort port = new LocalPort(12);
try {
    port.open();
} catch (PortDeviceException e) {
    reportError(e);
    logger.log(e.getMessage(), e);
} finally {
    ...
}
```

Our ```LocalPort``` is just a simple wrapper that catches and translates exceptions thrown by the ```ACMEPort``` class:

```java
public class LocalPort {
    private ACMEPort innerPort;

    public LocalPort(int portNumber) {
        innerPort = new ACMEPort(portNumber);
    }

    public void open() {
        try {
            innerPort.open();
        } catch (DeviceResponseException e) {
            throw new PortDeviceFailure(e);
        } catch (ATM1212UnlockedException e) {
            throw new PortDeviceFailure(e);
        } catch (GMXError e) {
            throw new PortDeviceFailure(e);
        }
    }

    ...
}
```

Wrapping third-party APIs is a best practice. When you wrap a third-party API, you minimize your dependencies upon it: You can choose to move to a different library in the future without much penality. Wrapping also makes it easier to mock out third-pary calls when you are testing your own code.

One final advantage of wrapping is that you aren't tied to a particular vendor's API design choices.

Often a sinle exception class is fine for a particular area of code. Use different classes only if there are times when you want to catch one exception and allow the other one to pass through.

## Define the Normal Flow

The process of following the steps of error handling defined here pushes error detection to the edges of your program. You wrap external APIs so that you can throw your own exceptions, and you define a handler above your code so that you can deal with any aborted computation. Most of the time this is a great approach, but there are some times when you may not want that.

### MealExpenses Example

```java
try {
    MealExpenses expenses = expensesReportDao.getMeals(employee.getId());
    m_total += expenses.getTotal();
} catch (MealExpensesNotFound e) {
    m_total += getMealPerDiem();
}
```

In this business, if meals are expensed, they become part of the total. If they aren't, the employee gets a meal *per diem* amount for that day. The exception clutters the logic.

Wouldn't be better if we didn't have to deal with the special case?

```java
MealExpenses expenses = expensesReportDao.getMeals(employee.getId());
m_total += expenses.getTotal();
```

We can change ```ExpensesReportDao``` so that it always returns a ```MealExpenses``` object. If there are no meal expenses, it returns a ```MealExpenses``` object that returns the _per diem_ as its total:

```java
public class PerDiemMealExpenses implements MealExpenses {
    public int getTotal() {
        // return the per diem default
    }
}
```

This is called the **Special Case Pattern**. You create a class or configure an object so that it handles a special case for you. When you do, the client code doesn't have to deal with exceptional behavior. That behavior is encapsulated in the special case object.

## Don't Return ```null```

```java
public void registerItem(Item item) {
    if (item != null) {
        ItemRegistry registry = persistentStore.getItemRegistry();
        if (registry != null) {
            Item existing = registry.getItem(item.getId());
            if (existing.getBillingPeriod().hasRetailOwner()) {
                existing.register(item);
            }
        }
    }
}
```

When we return ```null```, we are essentially creating work for ourselves and foisting problems upon our callers. All it takes is one missing ```null``` check to send an application spinning out of control.

If you are tempted to return ```null``` from a method, consider throwing an exception or returning a Special Case object instead. If are calling a ```null```-returning method from a third-party API, consider wrapping the method with a method that either throws an exception or returns a special case object.

### Solving the problem with the Special Case Pattern

Imagine you have code like this:

```java
List<Employee> employees = getEmployees();
if (employees != null) {
    for (Employee e : employees) {
        totalPay += e.getPay();
    }
}
```

Right now, ```getEmployees``` can return ```null```, but does it have to? If we change ```getEmployee``` so that it returns an empty list, we can cleanup the code:

```java
List<Employee> employees = getEmployees();
for (Employee e : employees) {
    totalPay += e.getPay();
}
```

Fortunately, Java has ```Collections.emptyList()```, and it returns a predefined immutable list that we can use for this purpose:

```java
public List<Employee> getEmployees() {
    if(<there are no employees>) {
        return Collections.emptyList();
    }
    ...
}
```

## Don't Pass ```null```

Unless you are working with an API which expects you to pass ```null```, you should avoid passing ```null``` in your code whenever possible.

### MetricsCalculator example

```java
public class MetricsCalculator {
    public double xProjection(Point p1, Point p2) {
        return (p2.x - p1.x) * 1.5;
    }
    ...
}
```

If someone passes ```null``` as an argument:

```java
xProjection(null, new Point(12, 14));
```

We'll get a ```NullPointerException```. How can we solve this?

- By creating and throwing a new exception type:

  ```java
  public class MetricsCalculator {
      public double xProjection(Point p1, Point p2) {
          if (p1 == null || p2 == null) {
              throw new InvalidArgumentException(
                  "Invalid argument for MetricsCalculator.xProjection"
              );
          }
          return (p2.x - p1.x) * 1.5;
      }
  }

- By using a set of assertions:

  ```java
  public class MetricsCalculator {
      public double xProjection(Point p1, Point p2) {
          assert p1 != null : "p1 should not be null";
          assert p2 != null : "p2 should not be null";
          return (p2.x - p1.x) * 1.5;
      }
  }
  ```

In most programming languages, there is no good way to deal with a ```null``` that is passed by a caller accidentaly. Because this is the case, the rational approach is to forbid passing ```null``` by default.
