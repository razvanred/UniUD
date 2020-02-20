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
