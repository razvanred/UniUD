# Enforce noninstantiability with a private constructor

Occasionally, you'll want to write a class that is just a grouping of static methods and static fields:

* It can be used to group related methods on primitive values or arrays, in the manner of ```java.lang.Math``` or ```java.util.Arrays```.
* It can be used to group static methods, including [factories](01_static_factory_methods.md), for objects that implement some interface, in the manner of ```java.util.Collections```.
* It can be used to group methods on a final class, since you can't put them in a subclass.

Such utility classes were not designed to be instantiated. In the absence of explicit constructor, however, the compiler provides a public, parameterless default constructor.

<mark style="background-color: lightgreen">Attempting to enforce noninstantiability by making a class abstract does not work.</mark> The class can be subclassed and the subclass instantiated. Furthermore, it mesleads the user into thinking the class was designed for [inheritance](../04_Classes_and_Interfaces/19_design_and_document_for_inheritance_or_else_prohibit_it.md).

A default constructor is generated only if a class contains no explicit constructors, so <mark style="background-color: #ffcc00">a class can be made noninstantiable by including a private constructor.</mark>

```java
// Noninstantiable utility class
public class UtilityClass {
    private UtilityClass() {
        throw new AssertionError();
    }
}
```

The assertion error isn't strictly required, but it provides insureance in case the constructor is accidentally invoked from within the class.

It is wise to provide a comment, because the constructor is provided expressly so that it cannot be invoked.

All constructors must invoke a superclass constructor, explicitly or implicitly, and this idiom prevents the class for being subclassed.
