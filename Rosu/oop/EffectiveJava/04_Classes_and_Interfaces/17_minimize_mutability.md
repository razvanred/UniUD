# Minimize Mutability

An immutable class is simply a class whose instances cannot be modified. During the lifecycle of the object, no change of the information in the instance can ever be observed. The Java platform libraries contain many immutable classes, including ```String```, the boxed primitive classes, and ```BigInteger``` and ```BigDecimal```. **Classes should be immutable unless there's a very good reason to make them mutable**.

## Steps to make a class immutable

1. **Don't provide _mutators_, methods that change the object's state**.
2. **Ensure the class cannot be extended**. Preventing subclassing is generally accomplished by making the class final.
3. **Make all fields final**. It is necessary to ensure correct behavior if a reference to a newly created instance is passed from one thread to another without syncronization.
4. **Make all fields private**.
5. **Ensure exclusive access to any mutable components.** Never initialize such a field to a client-provided object reference or return the field from an accessor. Make [defensive copies](../08_Methods/50_make_defensive_copies_when_needed.md) in constructors, accessors, and [```readObject``` methods](../12_Serialization/88_write_readObject_methods_defensively.md).

## Functional programming

```java
public final class Complex {

    private final double re;
    private final double im;

    public Complex(final double re, final double im) {
        this.re = re;
        this.im = im;
    }

    public double realPart() {
        return re;
    }

    public double imaginaryPart() {
        return im;
    }

    public Complex plus(final Complex c) {
        return new Complex(re + c.re, im + c.im);
    }

    public Complex minus(final Complex c) {
        return new Complex(re - c.re, im - c.im);
    }

    public Complex times(final Complex c) {
        return new Complex(
            re * c.re - im * c.im,
            re * c.im + im * c.re
        );
    }

    public Complex dividedBy(final Complex c) {
        final double temp = c.re * c.re + c.im * c.im;
        return new Complex(
            (re * c.re + im * c.im) / temp,
            (im * c.re - re * c.im) / temp
        );
    }

    @Override
    public boolean equals(final Object o) {
        if(o == this) {
            return true;
        }
        if(!(o instanceof Complex)) {
            return false;
        }
        final var complex = (Complex) o;
        return Double.compare(complex.re, re) == 0
            && Double.compare(complex.im, im) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(re, im);
    }

    @Override
    public String toString() {
        return String.format("%f + %fi", re, im);
    }
}
```

In this example, the arithmetics operations create and return a new ```Complex``` instance rather than modifying this instance. This pattern is known as the _funcional_ approach because methods return the result of applying a function to their operand, without modifying it. Constrast it to the _procedural_ or _imperative_ approach in which methods apply a procedure to their operand, causing its state to change. Note that the method names are propositions (such as ```plus```) rather than verbs (such as ```add```). This emphasizes the fact that methods don't change the values of the objects. The ```BigInteger``` and ```BigDecimal``` classes did _not_ obey this naming convention, and it led to many usage errors.

### Advantages of the functional approach

The functional approach enables immutability, which has many advantages:

* **Immutable objects are simple**. An immutable object can be in exactly one state, the state in which it was created. If you make sure that all constructors establish class invariants, then it is guarranteed that these invariants will remain true for all time.
* **Immutable objects are inherently thread-safe; they require no synchronization**.
* **Immutable objects can be shared freely**. Immutable classes should therefore encourage clients to reuse existing instances wherever possible. One easy way to do this is to provide public static final constrants for commonly used values:

  ```java
  public static final Complex ZERO = new Complex(0, 0);
  public static final Complex ONE = new Complex(1, 0);
  public static final Complex I = new Complex(0, 1);
  ```

  An immutable class can provide [static factories](../02_Creating_And_Destroying_Objects/01_static_factory_methods.md) that cache frequently requested instances to avoid creating new instances when existing ones would do. All the boxed primitive classes and ```BigInteger``` do this.

  A consequence of this advantange is that you never have to make [defensive copies](../08_Methods/50_make_defensive_copies_when_needed.md). You need not and should not provide [a ```clone``` method or a copy constructor](../03_Methods_Common_to_All_Objects/13_override_clone_judiciously.md) on an immutable class.

* **Not only can you share immutable objects, but they can share their internals**.
* **Immutable objects make great building blocks for other objects**. Immutable objects make great map keys and set elements: you don't have to worry about their values changing once they're in the map or set, which would destroy the map or set's invariants.
* **Immutable objects provide [failure atomicity](../10_Exceptions/76_strive_for_failure_atomicity.md) for free**. Their state never changes, so there is no possibility of a temporary inconsistency.

### Disadvantages of the functional approach

The major disadvantage of immutable classes is that **they require a separate object for each distinct value**. Creating objects can be costly, especially if they are large.

Suppose you have a millin-bit ```BigInteger``` and you want to change its low-order bit:

```java
final BigInteger moby = ...;
moby = moby.flipBit(0);
```

The ```flipBit``` method creates a new ```BigInteger``` instance, also a millin bits long, that differs from the original in only one bit. The operation requires time and space proportional to the size of the ```BigInteger```. Contrast this to ```java.util.BitSet```. Like ```BigInteger```, ```BitSet``` represents an arbitrarily long sequence of bits, but unlike ```BigInteger```, ```BitSet``` is mutable.

```java
final BitSet moby = ...;
moby.flip(0);
```

The performance problem is magnified if you perform a multistep operation that generates a new object at every step, eventually discarding all objects except the final result.

There are two approaches to coping with this problem:

1. Guess which multistep operations will be commonly required and provide them as primitives. If a multistep operation is provided as a primitive, the immutable class does not have to create a separate object at each step. Internally, the immutable class can be arbitrarily clever.
  
   For example, ```BigInteger``` has a package-private mutable _companion class_ that it uses to speed up multistep operations such as modular exponentiation. It is much harder to use the mutable companion class than to use ```BigInteger```, for all of the reasons outlined earlier.

2. If you cannot guess which multistep operations can be required, your best bet is to provide a _public_ mutable companion class.

   The main example of this approach in the Java platform libraries is the ```String``` class, whose mutable companion is ```StringBuilder``` (and its obsolete predecessor, ```StringBuffer```).

### Tips

* Instead of making an immutable class ```final```, you can make all of its constructors private or package-private and add public [static factories](../02_Creating_And_Destroying_Objects/01_static_factory_methods.md) in place of the public constructors.

  ```java
  public class Complex {
      private final double im;
      private final double re;

      private Complex(final double re, final double im) {
          this.re = re;
          this.im = im;
      }

      public static Complex valueOf(final double re, final double im) {
          return new Complex(re, im);
      }
  }
  ```

  This approach is often the best alternative. It is the most flexible because it allows the use of multiple package-private implementation classes.

* If you write a class whose security dependes on the immutability of a ```BigInteger``` or ```BigDecimal``` argument from an untrusted client, you must check to see that the argument is a "real" ```BigInteger``` or ```BigDecimal```, rather than an instance of an untrusted subclass. If it is the latter, you must [defensively copy](../08_Methods/50_make_defensive_copies_when_needed.md) it under the assumption that it might be mutable.

  ```java
  public static BigInteger safeInstance(final BigInteger b) {
      return b.getClass() == BigInteger.class ? b : new BigInteger(b.toByteArray());
  }
  ```

* Some rules for immutability are a bit stronger than necessary and can be relaxed to improve performance. In truth, no method may produce an externally visible change in the object state; however, some immutable classes have one or more nonfinal fields in which they cache the results of expensive computations the first time they are needed.

  For example, [```PhoneNumber```](../03_Methods_Common_to_All_Objects/11_always_override_hashCode_when_you_override_equals.md)'s ```hashCode``` method computes the hash code the first time it's invoked and caches it in case it's invoked again.

  This tecnique, an example of [lazy initialization](../11_Concurrency/83_use_lazy_initialization_judiciously.md), is also used by ```String```.
