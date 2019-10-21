# Consider Static Factory Methods instead of constructors

The traditional way for a class to allow a client to obtain an instance is to provide a public constructor. A class can provide a public _static factory method_, which is a simply static method that returns an instance of the class.

Simple example from the ```Boolean``` class:

```java
public static Boolean valueOf(boolean b) {
    return b ? Boolean.TRUE : Boolean.FALSE;
}
```

Note: the static factory method is not the same as the _Factory Method_ pattern.

A class can provide its clients with static factory methods instead of, or in addition to, public constructors. Providing a static factory method instead of a public constructor has both advantages and disadvantages.

Advantages:

1. **Unlike constructors, Static factory methods have names.**
   A class can have only a single constructor with a given signature. Programmers have been known to get around this restriction by providing two constructors whose parameter list differ only in the order of the parameter types. This is a really bad idea. People reading the code that uses these constructors will not know what the code does without referring to the class documentation.

   <mark>In case where a class seems to require multiple constructors with the same signature, replace the constructors with static factory methods and carefully chosen names to highlight their differences.<mark>

2. **Unlike constructors, static factory methods are not required to create a new object each time they're invoked.**  
   This allows [immutable classes](../4_Cleasses_and_Interfaces/17_minimize_mutability.md) to use preconstructed instances, or to cache instances as they're constructed, and dispense them repeatedly to avoid creating unnecessary duplicate objects.

   The ```Boolean.valueOf(boolean)``` method illustrates this technique: it _never_ creates an object. This technique is similar to the _Flyweight_ pattern. It can greately improve performance if equivalent objects are requested often, especially if they are expensive to create.

   Classes that do this are said to be _instance-controlled_. There are several reasons to write instance-controlled classes:
   * Instance control allows a class to guarantee that it is a [singleton](./03_singleton.md) or [noninstantiable](./04_noninstantiability.md).
   * it allows an [immutable value class](../04_Cleasses_and_Interfaces/17_minimize_mutability.md) to make the guarantee that no two equal instances exists: ```a.equals(b)``` if only ```a == b```.

   This is the basis of the _Flyweight_ pattern.

3. **Unlike constructors, static factory methods can return an object of any subtype of their return type.**
   One application of this flexibility is that an API can return objects withot making their classes public. Hiding implementation classes in this fashion leads to a very compact API. This tecnique lends itself to [interface-based frameworks](../20_prefer_interfaces_to_abstract_classes.md), where interfaces provide natural return types for static factory methods.

   Prior to Java 8, interfaces couldn't have static methods. By convention, static factory methods for an interface named _Type_ were put in a [noninstantiable companion class](./04_noninstantiability.md) named _Types_.

   For example, the Java Collection Framework has 45 utility implementations of its interfaces, providing unmodifiable collections, synchronized collections, etc. Nearly all of these implementations are exported via static factory methods in one noninstantiable class (```java.util.Collections```). The classes of the returned objects are all nonpublic.

   The Collections Framework API is much smaller than it would have been had it exported 45 separated public classes, one for each implementation. It is not just the bulk of the API that is reduced but the _conceptual weight_: the number and the difficulty of the concepts that programmers must muster in order to use the API. <mark>The programmer knows that the returned obejct has precisely the API specified by its interface</mark>, so there is no need to read additional class documentation for the implementation class.

   Furthermore, using such a static factory method [requires the client to refer to the returned object by interface rather than implementation class](../09_General_Programming/64_refer_to_objects_by_their_interfaces.md).

   As of Java 8, the restriction that interfaces cannot contain static methods was eliminated, so there is typically little reason to provide a noninstantiable companion class for an interface. It may still be necessary to put the bulk of the implementation code behind these static methods in a separate package-private class. This is because Java 8 requires all static members of an interface to be public. Java 9 allows private static methods, but static fields and static member classes are still required to be public.

4. **The class of the returned object can bery from class to call as a function of the input parameters** (through _Overloading_).

5. **The class of the returned object need not exist when the class containing the method is written.**
   Such flexible static factory methods form the basis of _service provider framewors_ (like the Java Database Connectivity API).

   <mark>A **service provider framework** is a system in which providers implement a service, and the system makes the implementations available to the clients, decoupling the clients from the implementations.<mark>

   This are three essential components in a service provider framework:

   * a _service interface_, which represents an implementation;
   * a _provider registration API_, which provides use to register an implementations;
   * a _service access API_, which clients use to obtain instances of the service.
   * (optional) a _service provider interface_, which describes a factory object that produce instances of the service interface. In the absence of this component, implementations must be instantiated [reflectively](../09_General_Programming/65_prefer_interfaces_to_reflection.md).

   Note: [Dependency Injection frameworks](./05_prefer_dependency_injection_to_handwiring_resources.md) can be viewed as powerful service providers.

Limitations:

1. **By providing only static factory methods, classes without public or protected constructors cannot be subclassed.**
   Arguably this can be a blessing in disguise because [it encourages programmers to use composition instead of inheritance](../04_Classes_and_Interfaces/18_favor_composition_over_inheritance), and is required for [immutable types](../04_Cleasses_and_Interfaces/17_minimize_mutability.md).

2. **Static Factory Methods are hard for programmers to find.**
   They do not stand out in API documentation in the way that constructors do, so it can be difficult to fugure out how to instantiate a class that provides static factory methods instead of constructors.

   You can reduce this problem by drawing attention to static factories in class or interface documentation and by adhering to common naming conventions:

   * **from**: A type-conversion method that takes a single parameter and returns a corresponding instance of this type.

     ```java
     final Date d = Date.from(instant);
     ```

   * **of**: An aggregation method that takes multiple parameters and returns an instance of this type that incorporates them.

     ```java
     final Set<Rank> faceCards = EnumSet.of(JACK, QUEEN, KING);
     ```

   * **valueOf**: A more verbose alternative to ```from``` and ```of```.

     ```java
     final BigInteger prime = BigInteger.valueOf(Integer.MAX_VALUE);
     ```

   * **instance** or **getInstance**: Returns an instance that is described by its parameters (if any) but cannot be said to have the same value.

      ```java
      final StackWalker luke = StackWalker.getInstance(options);
      ```

   * **create** or **newInstance**: Like ```instance``` or ```newInstance```, except that the method guarantees that each call returns a new instance.

     ```java
     final Object newArray = Array.newInstance(classObject, arrayLen);
     ```

   * **get*Type***: Like ```getInstance```, but used if the factory method is in a different class. _Type_ is the type of object returned by the factory method.

     ```java
     final FileStore fs = Files.getFileStore(path);
     ```

   * **new*Type***: Like ```newInstance```, but used if the factory method is in a different class. _Type_ is the type of object returned by the factory method.

     ```java
     final BufferedReader br = Files.newBufferedReader(path);
     ```

   * ***type***: A concise alternative to get*Type* and new*Type*.

     ```java
     final List<Complaint> litany = Collections.list(legacyLitany);
     ```
