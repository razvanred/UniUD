# Objects and Data Structures

## Data Abstractino

Consider the two different points:

* ```ConcretePoint.java```:

  ```java
  public class Point {
      public double x;
      public double y;
  }
  ```

* ```AbstractPoint.java```:

  ```java
  public interface Point {
      double getX();
      double getY();
      void setCartesian(double x, double y);
      double getR();
      double getTheta();
      void setPolar(double r, double theta);
  }
  ```

The beautiful thing about ```AbstractPoint.java``` is that there is no way you can tell whether the implementation is in rectangular or polar coordinates. It might be neither! And yet the interface still unmistakably represents a data structure.

But it represents more than a data structure. The methods enforce an access policy. You can read the individual coordinates independently, but you must set the coordinates together as an atomic operation.

```ConcretePoint.java```, on the other hand, is very clearly implemented in rectangular coordinates, and it forces us to manipulate those coordinates independently. This exposes implementation Indeed, it would expose implementation even if the variables were private and we were using single variable getters and setters.

Hiding implemenation is about abstraction. A class does not simply push its variables out through getters and setters. Rather it exposes abstract interfaces that allow its user to manipulate the essence of data, without having to know its implementation.

Consider the two following ```Vehicle```s:

* ```ConcreteVehicle.java```:

  ```java
  public interface Vehicle {
      double getFuelTankCapacityInGallons();
      double getGallonsInGasoline();
  }
  ```

* ```AbstractVehicle.java```:

  ```java
  public interface Vehicle {
      double getPercentFuelRemaining();
  }
  ```

In the concrete case you can be pretty sure that these are just accessors of variables. In the abstract case, you have no clue at all about the form of the data.

In both of the cases the second option is preferrable. We do not want to expose the details of our dataa. Rather we want to express our data in abstract terms.

## Data/Object Anti-Symmetry

These two examples show the difference between **objects** and **data structures**.

> Objects hide their data behind abstractions and expose functions that operate on the data.

and,

> Data structure expose their data and have no meaningful function. They are virtual opposites.

Consider these two approaches:

* **Procedural**:

  ```java
  public class Square {
      public Point topLeft;
      public double side;
  }

  public class Rectangle {
      public Point topLeft;
      public double height;
      public double width;
  }

  public class Circle {
      public Point center;
      public double radius;
  }

  public class Geometry {
      public final double PI = 3.14;

      public double area(Object shape) throws NoSuchShapeException {
          if(shape instanceof Square) {
              Square s = (Square) shape;
              return s.side * s.side;
          }
          if(shape instanceof Rectangle) {
              Rectangle r = (Rectangle) shape;
              return r.heigth * r.width;
          }
          if(shape instanceof Circle) {
              Circle c = (Circle) shape;
              return c.radius * c.radius * PI;
          }
          throw new NoSuchShapeException();
      }
  }
  ```

* **Object Oriented**:

  ```java
  public class Square implements Shape {
      private Point topLeft;
      private double side;

      public double area() {
          return side * side;
      }
  }

  public class Rectangle implements Shape {
      private Point topLeft;
      private double width;
      private double height;

      public double area() {
          return width * height;
      }
  }

  public class Circle implements Shape {
      private static final double PI = 3.14;

      private Point center;
      private double radius;

      public double area() {
          return PI * radius * radius;
      }
  }
  ```

> Procedural code (code using data structures) makes it easy to add new functions without changing the existing data structures. OO code, on the other hand, makes it easy to add new classes without changing existing functions.

And also:

> Procedural code makes it hard to add new data structures because all the functions must change. OO code makes it hard to add new functions because all the classes must change.

For these cases objects and OO are most appropriate.

Mature programmers know that the idea that everything is an object is a myth. Sometimes you really do want simple data structures with procedures operating on them.

## The Law of Demeter

There is a well-known heuristic called the _Law of Demeter_ that says a module should not know about the innards of the object it manipulates.

Objects hide their data and expose operations. This means that an object should not expose its internal structure through accessors because to do so is to expose, rather than to hide, its internal structure.

The Law of Demeter says that a method ```f``` of a class ```C``` should only call the methods of these:

* ```C```
* An object created by ```f```
* An object passed as an argument to ```f```
* An object held in an instance variable of ```C```.

The method should not invoke methods on objects that are returned by any of the allowed functions. In other words, talk to friends, not to strangers.

The following code appears to violate the Law of Demeter because it calls the ```getScratchDir()``` function on the return value of ```getOptions()``` and then calls ```getAbsolutePath()``` on the return value of ```getScratchDir()```.

```java
final String outputDir = ctxt.getOptions().getScratchDir().getAbsolutePath();
```

### Train Wrecks

This kind of code is often called a _train wreck_ because it look like a bunch of coupled train cars. Chains of calls like this are generally considered to be sloppy style and should be avoided. It is usuall best to split them up as follows:

```java
Options opts = ctxt.getOptions();
File scratchDir = opts.getScratchDir();
final String outputDir = scratchDir.getAbsolutePath();
```

Are these two snippets of code violations of the Law of Demeter? Certainly the containing module knows that the ```ctxt``` object contains options, which contain a scratch directory, which has an absolute path. That's a lot of knowledge for one function to know. The calling function knows how to navigate through a lot of different objects.

Whether this is a violation of Demeter dependes on whether or not ```ctxt```, ```Options``` and ```ScratchDir``` are objects or data structures. If they are objects, then their internal structure shoud be hidden rather than exposed, and so knowledge of their innards is a clear violation of the Law of Demeter. On the other hand, if ```ctxt```, ```Options``` and ```ScratchDir``` are just data structures with no behavior, then they naturally expose their internal structure, ans so Demeter does not apply.

The use of accessor functions confuses the issue. If the code had been written as follows, then we probably wouldn't be asking about Demeter violations.

```java
final String outputDir = ctxt.options.scratchDir.absolutePath;
```

This issue would be a lot less confusing if data structures simply had public variables and no functions, whereas objects had private variables and public functions. However there are frameworkds and standards (e.g., "beans") that demand that even simple data structures have accessors and mutators.

### Hybrids

This confusion sometimes leads to unfortunate hybrid structures that are half object and half data structure. They have functions that do significant things, and they also have either public variables or public accessors and mutators that, for all intents and purpouses, make the private variables public, tempting other external functions to use those variables the way a procedural program would use a data structure.

Such hybrids make it hard to add new functions but also make it hard to add new data structures. They are the worst of both worlds. Avoid creating them. They are indicative of a muddled design whose authors are unsure of - or worse, ignorant of - whether they need protection from functions or types.

### Hiding Structure

What if ```ctxt```, ```options``` and ```scratchDir``` are objects with real behavior? Then, because objects are supposed to hide their internal structure, we should not be able to navigate through them. How we get the absolute path of the scratch directory?

```java
ctxt.getAbsolutePathOfScratchDirectoryOption();
```

or

```java
ctxt.getScratchDirectoryOption().getAbsolutePath();
```

The first option could lead to an explosion of methods in the ```ctxt``` object. The second presumes that ```getScratchDirectoryOption()``` returns a data structure, not an object. Neither option feels good.

If ```ctxt``` is an object, we should be telling it to do something; we should not be asking it about its internals. So why did we want the absolute path of the scratch directory? What were we going to do with it?

Consider this code from the same module:

```java
String outFile = outputDir + "/" + className.replace('.', '/') + ".class";
FileOutputStream fout = new FileOutputStream(outFile);
BufferedOutputStream bos = new BufferedOutputStream(fout);
```

The admixture of different levels of detail is a bit troubling. Ignoring the dots, slashes, file extensions mixed with the ```File``` objects with the enclosing code, we see that the intent of getting the absolute path of the scratch directory was to create a scratch file of a given name.

What if we to told the ```ctxt``` object to do this?

```java
BufferedOutputStream bos = ctxt.createScratchFileStream(classFileName);
```

This allows ```ctxt``` to hide its internals and prevents the current function from having to violate the Law of Demeter by navigating through objects it shouldn't know about.

## Data Transfer Object

A **D**ata **T**ransfer **O**bject is a form of data structure with public variables and no functions.

```java
public class Point {
    public double x;
    public double y;
}
```

Somewhat more common is the "bean" form. Beans have private variables manipulated by getters and setters.

```java
public class Point {
    private final double x;
    private final double y;

    public Point(final double x, final double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }
}
```

### Active Record

Active Records are special forms of DTOs. They are data structures with public (or bean-accessed) variables; but they typically have navigational methods like ```save``` and ```find```. Typically these Active Records are direct translations from database tables, or other data sources.

* Treat the Active Record as a data structure
* Create separate objects that contain business rules and hide their internal data

## Conclusion

* **Objects expose behavior and hide data**. This makes it easy to add new kinds of objects without changing existing behaviors. It also makes it hard to add new behaviors to existing objects.
* **Data structures expose data and have no significant behavior**. This makes it easy to add new behaviors to existing data structures but makes it hard to add new data structures to existing functions.
