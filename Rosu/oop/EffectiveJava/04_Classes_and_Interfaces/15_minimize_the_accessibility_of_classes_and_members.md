# Minimize the accessibilty of classes and members

A well-designed componennt hides all its implementation details, cleanly separating its API from its implementation. Components then communicate only through their APIs and are oblivious to each other inner workings. This concept, known as **information hiding** or _encapsulation_, is a fundamental tenet of software design.

Information hiding is important for many reasons:

- It _decouples_ the components that comprise a system, allowing them to be developed in isolation, in parallel.
- It does not, it and of itself, cause good performace, but it enables effective performance tuning: once a system is complete and [profiling has determined which components are causing performance problems](../09_General_Programming/67_optimize_judiciously.md), those components can be optimized without affecting the correctness of others.
- It increases software reuse because components that aren't tightly coupled often prove useful in other contexts besides the ones for which they were developed.
- It decreases the risk in building large systems because individual components may prov3e successful even if the system does not.

## Access control mechanism

The [access control mechanism](https://docs.oracle.com/javase/specs/jls/se7/html/jls-6.html#jls-6.6) specifies the _accessibility_ of classes, interfaces and members. The accessibility of an entity is determined by the location of its declaration and by which, if any, of the access modifiers (```private```, ```protected``` and ```public```) is present on the declaration.

**Make each class or member as inaccessible as possible:** use the lowest possible access level consistent with the proper functioning of the software that you are writing.

### Top-level classes

For top-level (non-nested) classes and interfaces, there are only two possible access levels:

- **Package-private**: class or interface defined without any access level modifier, visible only in the same working package. The class is part of the implementation rather than the exported API, and you can modify it, replace it, or eliminate it in a subsequent release without fear of harming existent clients.
- **Public**: class or interface defined with the ```public``` modifier, visible from any package. You are obligated to support the class forever to maintain compatibility.

A few tips:

- If a top-level class or interface can be made package-private, it should be.
- If a package-private top-level class or interface is used by only one class, consider making the top-level class a private [static nested class](./24_favor_static_member_classes_over_nonstatic.md) of the sole class that uses it.

### Members (fields, methods, nested classes, nested interfaces)

For members there are four possible access levels:

- **private**: The member is accessible only from the top-level class where it is declared.
- **package-private**: The member is accessible from any class in the package where it is declared. Technically known as _default access_, this is the access level you get if no access modifier is specified (except for interface members, which are public by default).
- **protected**: The member is accessible from subclasses of the class where it is declared and from any class in the package where it is declared.
- **public**: The member is accessible from anywhere.

The package-private or private members can "leak" into the exposed API if the class implements ```Serializable```.

A protected member is part of the class's exported API and must be supported forever. A protected member of an exported class represents a public commitment to an implementation detail. The need for protected members should be relatively rare.

**[If a method overrides a superclass method, it cannot have a more restrictive access level in the subclass than in the superclass](https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4.8.3):** an instance of the subclass is usable anywhere that an instance of the superclass is usable (the [Liskov substitution principle](../03_Methods_Common_to_All_Objects/10_obey_the_general_contract_when_overriding_equals.md)). For example, if a class implements an interface, all of the class methods that are in the interface must be declared public in the class.

#### Testing

To facilitate testing your code, you may be tempted to make a class, interface or member more accessible than otherwise necessary. It is acceptable to make a private member of a public class package-private in order to test it, but it is not acceptable to raise the accessibility any higher.

**It is not acceptable to make a class, interface, or member a part of package's exported API to facilitate testing.**

Luckily, it isn't necessary either because tests can be made to run as part of the package being tested, thus gaining access to its package-private elements.

#### Public instnace fields

**[Instance fields of public classes should rarely be public.](./16_in_public_classes_use_accessor_methods_not_public_fields.md)** If an instance field is nonfinal or is a reference to a mutable object, then making it public, you give up the ability to limit the values that can be stored in the field.

You give up:

- the ability to enforce invariants involing the field;
- the ability to to take any action when the field is modified.

So, **classes with public mutable fields are not generally thread-safe**. Even if a field is final and refers to an immutale object, by making it public you give up the flexibility to switch to a new internal data representation in which the field does not exist.

The same advice applies to static fields, with one exception. You can expose constants via public static final fields, assuming the constants form an integral part of the abstraction provided by the class. [By convention, such fields have names consisting of capital letters, with words separated by underscores](../09_General_Programming/68_adhere_to_generally_accepted_naming_conventions.md). It is critical that these fields contain either primitive values or references to [immutable objects](./17_minimize_mutability.md). While the reference cannot be modified, the reference object can be modified - with disastrous results.

Note that a nonzero-length array is always mutable, so **it is wrong for a class to have a public static final array field, or an accessor that returns such a field**.

```java
// Potential security hole!
public static final Object[] VALUES = { ... };
```

There are two ways to fix the problem:

- By making the public array private and add a public immutable list:

  ```java
  private static final Object[] PRIVATE_VALUES = { ... };
  public static final List<Object> VALUES = Collections.unmodifiableList(Arrays.asList(PRIVATE_VALUES));
  ```

- By making the array private and add a public method that returns a copy of a private array:

  ```java
  private static final Object[] PRIVATE_VALUES = { ... };
  public static Object[] values() {
      return PRIVATE_VALUES.clone();
  }
  ```

## Module system

As of Java 9, there are two additional, implicit access levels introducted as part of the _module system_.

= A module is a grouping of packages, like a package is a grouping of classes.
= A module may explicitly export some of its packages via _export declarations_ in its _module declaration_ (which is by convention contained in a source file named ```module-info.java```).

### Access control

Public and protected members of unexported packages in a module are inaccessible outside the module; within the module, accessibility is unaffected by export declarations. Using the module system allows you to share classes among packages within a module without making them visible to the entire world.

Public and protected members of public classes in unexported packages give rise to the two implicit access levels, which are intramodular analogues of the normal public and protected levels. The need for this kind of sharing is relatively rare and often be eliminated by rearranging the classes within the packages.

Unlike the four main access levels, the two module-based levels are largely advisory. If you place a module's JAR file on your application's class path instead of its module path, the packages in the module revert to therir non-modular behavior: all of the public and protected members of the packages' public classes have their normal accessibility, regardless of whether the packages are exported by the module. The one place where the newly introduced access levels are strictly enforced is the JDK itself: the unexported packages in the Java libraries are truly inaccessible outside of their modules.
