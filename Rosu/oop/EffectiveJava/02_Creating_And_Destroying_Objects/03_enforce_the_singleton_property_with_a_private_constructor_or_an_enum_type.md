# Enforce the singleton property with a private constructor or an enum type

A _singleton_ is simply a class that is instantiated exactly once.

<mark>Making a class a singleton can make it difficult to test its clients</mark> because it's impossible to substitute a mock implementation for a singleton class unless it implements an interface that serves as its type.

There are two common ways to implement singletons, and both are based on keeping the constructor private and exporting a static member to provide access to the sole instance:

* (preferable approach) Singleton with public final field:

  ```java
  public class Elvis {
      public static final Elvis INSTANCE = new Elvis();

      private Elvis() { }

      public void leaveTheBuilding() { }
  }
  ```

  The lack of public or protected constructor guarantees a _"mono-elvistic"_ universe: no more, no less.
  Nothing that a client does can change this, with one caveat: a priviledged client can invoke the private constructor [reflectively](../09_General_Programming/65_prefer_interfaces_to_reflection.md) with the aid of ```ÀccessibleObject.setAccessible``` method. If you need to defend against the attack, modify the constructor to make it throw an exception if it's asked to create a second instance.

  Advantages:

  1. The API makes it clear that the class is a singleton;
  2. It's simpler.

* Singleton with static factory:

  ```java
  public class Elvis {
      private static final Elvis INSTANCE = new Elvis();
  
      private Elvis() { }
  
      public void leaveTheBuilding() { }
  
      public static Elvis getInstance() { return INSTANCE; }
  }
  ```

  All calls to ```Elvis.getInstance```return the same object reference, and no other ```Èlvis``` instance will ever be created (with the same caveat mentioned earlier).
  
  Advantages:

  1. It gives you the flexibility to change your mind about whether the class is a singleton without changing its API. The factory method returns the sole instance, but it could be modified to return, say, a separate instance for each thread that invokes it.
  2. You can write a [_generic singleton factory_](../05_Generics/30_favor_generic_methods.md) if your application requires it.
  3. A method reference can be used as a supplier, for example ```Èlvis::instance``` is a ```Supplier<Elvis>```.
  
To make a singleton class _serializable_ is not sufficient merely to add ```implements Serializable``` to its declaration. To maintain the singleton guarantee, declare all instance fields ```transient``` and provide a ```readResolve``` method.

## Enum Singleton

A third way to implement a singleton is to declare a single-element enum:

```java
public enum Elvis {
    INSTANCE;

    public void leaveTheBuilding() { }
}
```

Advantages:

1. More concise
2. Provides the serialization machinery for free
3. Provides an ironclad guarantee against multiple instantiation

<mark>A single-element enum type is often the best way to implement a singleton</mark>.

Note: you can't use this approach if your singleton must extend a superclass other than ```Enum``` (but you can declare an enum to implement interfaces).
