# Obey the general contract when overriding ```equals```

## Avoiding to override ```equals```

If you avoid to override the ```equals``` method, then each instance of the class is equal only to itself.

This is the right thing to do if any of the following conditions apply:

* **Each instance of the class is inherently unique.** This is true for classes such as ```Thread``` that represent active entities rather than values. The ```equals``` implementation provided by the ```Object``` has exactly the right behavior for these classes.
* **There is no need for the class to provide a "logical equality" test** like the ```Pattern``` class.
* **A superclass has already overridden ```equals```, and the superclass behaviour is appropriate for this class.** For example, most ```Set``` implementations inherit their equals implementation from ```AbstractSet```, ```List``` implementations from ```AbstractList``` and ```Map``` implementations from ```AbstractMap```.
* **The class is private or package-private, and you are certain that its ```equals``` method will never be invoked.** You can override the ```equals``` method in this way to ensure that it isn't invoked accidentally:

```java
@Override
public boolean equals(Object o) {
    throw new AssertionError();
}
```

One kind of value class that does _not_ require the ```equals``` method to be overriden is a class that uses instance control to ensure that at most one object exists with each value. [Enum types](../06_Enums_and_Annotations/34_use_enums_instead_of_int_constants.md) fall into this category. For these classes, logical equality is the same as object identity, so ```Object```'s ```equals``` method functions as a logical ```equals``` method.

## When it is appropriate to override ```equals```

It is appropriate to override the ```equals``` method when a class has a notion of _logical equality_ that differs from mere object identity and a superclass has not already overridden ```equals```.

This is generally the case for _value classes_. A value class is simply a class that represents a value, such as ```Integer``` or ```String```.

Also, it enables instances to serve as map keys or set elements with predictable, desirable behavior.

## General contract of the ```equals``` method

The ```equals``` method implements an _equivalence relation_. It has to to be: _reflexive_, _symmetric_, _transitive_, _consistent_ and non-equal to ```null```.

### Reflexivity

> For any not-null reference value ```x```, ```x.equals(x)``` must return ```true```.

If you were to violate this property and then add an instance of your class to a collection, the ```contains``` method might well say that the collection didn't contain the instance that you just added.

### Symmetry

> For any not-null reference values ```x``` and ```y```, ```x.equals(y)``` must return ```true``` if and only if ```y.equals(x)``` returns ```true```.

Consider the following class, which implements a case-insensitive string. The case of the string is preserved by ```toString``` but ignored in ```equals``` comparison.

```java
// Broken - violates simmetry!
public final class CaseInsensitiveString {
    private final String s;

    public CaseInsensitiveString(final String s) {
        this.s = Objects.requireNotNull(s);
    }

    @Override
    public boolean equals(final Object other) {
        if(other instanceof CaseInsensitiveString) {
            return s.equalsIgnoreCase(((CaseInsensitiveString) other).s);
        }
        if(other instanceof String) { // One-way interoperability!
            return s.equalsIgnoreCase((String) other);
        }
        return false;
    }
}
```

The well-intentionated ```equals``` method in this class naively attempts to interoperate with ordinary strings.

```java
final var cis = new CaseInsensitiveString("Polish");
final String s = "polish";
```

As expected, ```cis.equals(s)``` returns ```true```. The problem is that while the ```equals``` method in ```CaseInsensitiveString``` knows about ordinary strings, the ```equals``` method in ```String``` is oblivious to case-insensitive strings. Therefore, ```s.equals(cis)``` returns ```false```, a clear violation of symmetry.

Consider the example:

```java
List<CaseInsensitiveString> list = new ArrayList<>();
list.contains(s);
```

We don't know what it will return the ```contains``` method defined in the ```ArrayList``` class, it depends by the JDK implementation (it could also throw a ```RuntimeException```).

<mark>Once you've violated the ```equals``` contract, you simply don't know how other objects will behave when confronted with your object.</mark>

To eliminate the problem, remove the attempt to interoperate with ```String``` from the ```equals``` method.

```java
@Override
public boolean equals(final Object other) {
    return (other instanceof CaseInsensitiveString)
            && ((CaseInsensitiveString) other).s.equalsIgnoreCase(s);
}
```

### Transitivity

> For any non-null reference values ```x```, ```y``` and ```z```, if ```x.equals(y)``` returns ```true``` and ```y.equals(z)``` returns ```true```, then ```x.equals(z)``` **must** return ```true```.

Consider the case of a subclass that adds a new _value component_ to its superclass. The subclass adds a piece of information that affects ```equals``` comparison.

```java
public class Point {
    private final int x;
    private final int y;

    public Point(final int x, final int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(!(o instanceof Point)) {
            return false;
        }
        final var p = (Point) o;
        return p.x == x && p.y == y;
    }
}
```

Suppose you want to extend the class:

```java
public class ColorPoint extends Point {
    private final Color color;

    public ColorPoint(final int x, final int y, final Color color) {
        super(x, y);
        this.color = color;
    }
}
```

If you leave the ```ColorPoint``` class without its own ```equals``` implementation, the implementation is inherited from ```Point``` and color information is ignored in the ```equals``` comparison.

#### Violating symmetry

```java
// Broken - Violates simmetry!
@Override
public boolean equals(Object other) {
    if(!(o instanceof ColorPoint)) {
        return false;
    }
    return super.equals(other) && ((ColorPoint) other).color == color;
}
```

The problem with this method is that you might get different results when compairing a point to a color point and vice versa. The former comparison ignores color, while the latter comparison always returns ```false``` because the type of the argument is incorrect.

```java
final var p = new Point(1, 2);
final var c = new ColorPoint(1, 2, Color.RED);

p.equals(c); // true
c.equals(p); // false
```

#### Violating transitivity

Trying to fix the symmetry problem above with a new implementation:

```java
// Broken - Violates transitivity!
@Override
public boolean equals(Object other) {
    if(!(o instanceof Point)) {
        return false;
    }
    if(!(o instanceof ColorPoint)) {
        return o.equals(this);
    }
    return super.equals(o) && ((ColorPoint) other).color == color;
}
```

This approach does provide symmetry, but at the expense of transivity:

```java
final var c1 = new ColorPoint(1, 2, Color.GREEN);
final var p = new Point(1, 2);
final var c2 = new ColorPoint(1, 2, Color.BLUE);

c1.equals(p); // true
p.equals(c2); // true
c2.equals(c1); // false - clear violation of transitivity!
```

This approach can cause infinite recursion, when compairing two different subclasses of ```Point``` with their own ```equals``` implementation.

#### Liskov Substitution Principle

This is a fundamental problem of equivalence relations in object-oriented languages.

<mark>There is no way to extend an instantiable class and add a value component while preserving the ```equals``` contract</mark>, unless you're willing to forget the benefits of object-oriented abstraction.

```java
// Broken - Violates Liskov substitution principle
@Override
public boolean equals(Object o) {
    if(o == null || o.getClass() != getClass()) {
        return false;
    }
    final var p = (Point) o;
    return p.x == x && p.y == y;
}
```

This has the effect of equating objects only if they have the same implementation class. This may not seem so bad, but the consequences are unacceptable. An instance of a subclass of ```Point``` is still a ```Point```, and it still needs to function as one but if fails to do so if you take this approach.

<mark style="background-color: lightgreen">The _Liskov Substitution Principle_ says that any important property of a type should also hold for all its subtypes so that any method written for the type should work equally well on its subtypes</mark>.

```java
private static final Set<Point> unitCircle = Set.of(
    new Point(1, 0), new Point(0, 1), new Point(-1, 0), new Point(0, -1)
);

public static boolean onUnitCircle(final Point p) {
    return unitCircle.contains(p);
}
```

Suppose we extend ```Point``` in some way that doesn't add a value component:

```java
public class CounterPoint extends Point {
    private static final AtomicInteger counter = new AtomicInteger();

    public CounterPoint(final int x, final int y) {
        super(x, y);
        counter.incrementAndGet();
    }

    public static int numberCreated() {
        return counter.get();
    }
}
```

The statement said that a subclass of ```Point``` (such as ```CounterPoint```) is still a ```Point``` and must act as one. If we pass a ```CounterPoint``` to the ```onUnitCircle``` method, the latter will return ```false``` regardless of the ```CounterPoint``` instance's ```x``` and ```y``` coordinates. Most collections use the ```equals``` method to test for containment.

#### [Favor composition over inheritance](../04_Classes_and_Interfaces/18_favor_composition_over_inheritance.md)

Instead of having the ```ColorPoint``` extend ```Point```, give ```ColorPoint``` a private ```Point``` field and [a public _view_ method](../02_Creating_And_Destroying_Objects/06_avoid_creating_unnecessary_objects.md) that returns the point at the same position as this color point:

```java
public class ColorPoint {
    private final Point point;
    private final Color color;

    public ColorPoint(final int x, final int y, final Color color) {
        point = new Point(x, y);
        this.color = Objects.requireNonNull(color);
    }

    public Point asPoint() {
        return point;
    }

    @Override
    public boolean equals(Object o) {
        if(o == this) {
            return true;
        }
        if(!(o instanceof ColorPoint)) {
            return false;
        }
        final var c = (ColorPoint) o;
        return c.point.equals(point) && c.color.equals(color);
    }
}
```

Note: you _can_ add a value component to a subclass of an _abstract_ class without violating the ```equals``` contract.

You could have an abstract class ```Shape``` with no value components, a subclass ```Circle``` that adds a ```radius``` field, and a subclass ```Rectangle``` that adds ```height``` and ```width``` fields. Problems of the sort shown earlier won't occur so long as it is impossible to create a superclass instance directly.

### Consistency

> For any non-null reference values ```x``` and ```y```, multiple invocations of ```x.equals(y)``` must consistently return ```true``` or consistently return ```false```, provided no information used in ```equals``` comparison is modified.

Mutable objects can be equal to different objects at different times while immutable objects can't. Whether or not a class is immutable, **do not write an ```equals``` method that dependes on unreliable resources**.

For example, ```java.net.URL```'s ```equals``` method relies on comparison of the IP address associated with the URLs. Translating a host name to an IP address can require network access, and it's not guaranteed to yield the same results over time. The behaviour of ```URL```'s ```equals``` method was a big mistake and should not be emulated.

### Not-nullity

> For any non-null reference value ```x```, ```x.equals(null)``` must return ```false```.

```java
@Override
public boolean equals(final Object o) {
    if(o == null) { // Unnecessary test
        return false;
    }
    ...
}
```

[The ```instanceof``` operator is specified to return ```false``` if its first operand is ```null```, regardless of what type appears in the second operand](https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.20.2). Therefore, the type check will return ```false``` if ```null``` is passed in, so you don't need an explicit ```null``` check.

## High-quality ```equals``` method

1. **Use the ```==``` operator to check if the argument is a reference to ```this``` object**.
2. **Use the ```instanceof``` operator to check if the argument has the correct type**. Use an interface if the class implements an interface that redefines the ```equals``` contract to permit comparison across classes that implement the interface. Collection interfaces such as ```Set```, ```List```, ```Map``` and ```Map.Entry``` have this property.
3. **Cast the argument to the correct type**. At this point it is guaranteed to succeed.
4. **For each "significant" field in the class, check if that field of the argument matches the corresponding field of this object**.
   * For primitive fields whose type is not ```float``` or ```double```, use the ```==``` operator for comparison;
   * For object fields, call the ```equals``` method recursively;
   * For ```float``` and ```double``` fields, use the static ```Float.compare(float, float)``` and ```Double.compare(double, double)``` methods. This special treatment of ```float``` and ```double``` fields is made necessary due to the existence of ```Float.NaN```, ```-0.0f``` and the analogous ```double``` values. While you could compare ```float``` and ```double``` fields with the static methods ```Float.equals``` and ```Double.equals```, this would entail autoboxing on every comparison, which would have poor performance.
   * For array fields, if every element in the array field is significant, use one of the ```Arrays.equals``` methods;
   * For nullable object fields, use the static ```Objects.equals(Object, Object)``` method: when both objects are ```null``` it returns ```true```;
   * For some classes, fields comparison are more complex than simple equality test: if this is the case, you may want to store a _canonical form_ of the field so the ```equals``` method can do a cheap exact comparison on canonical forms rather than a more costly nonstandard comparison. This technique is most appropriate for [immutable classes](../04_Classes_and_Interfaces/17_minimize_mutability.md). If the object can change, you must keep the canonical form up to date.
   * You must not compare fields that are not part of an object's logical state, such as lock fields to synchronize operations.
   * Avoid compairing derivated fields, which can be calculated from "significant fields", but doing so may improve the performance of the ```equals``` method.

```java
public final class PhoneNumber {
    private final short areaCode, prefix, lineNum;

    public PhoneNumber(final short areaCore, final short prefix, final short lineNum) {
        this.areaCode = areaCode;
        this.prefix = prefix;
        this.lineNum = lineNum;
    }

    @Override
    public boolean equals(Object o) {
        if(o == this) {
            return true;
        }
        if(!(o instanceof PhoneNumber)) {
            return false;
        }
        final var p = (PhoneNumber) o;
        return p.areaCode == areaCode && p.prefix == prefix && p.lineNum == lineNum;
    }
}
```

## Tips

* [Always override ```hashCode``` when you override ```equals```](11_always_override_hashCode_when_you_override_equals.md).
* Don't try to be clever: if you simply test fields for equality, it's not hard to adhere the ```equals``` contract.
* Don't substitute another type for ```Object``` in the ```equals``` declaration. [Consistent ```Override``` annotation will prevent you from making this mistake](../06_Enums_and_Annotations/40_consistently_use_the_Override_annotation.md) and the ```equals``` method won't compile.
* [AutoValue framework](https://github.com/google/auto/tree/master/value) is an alternative to writing and testing the ```equals``` and ```hashCode``` methods manually:

  ```java
  @AutoValue
  public class Circle {
      private final int radius;
  
      public Circle(final int radius) {
          this.radius = radius;
      }
  }
  ```

  This library automatically generates these methods for you, triggered by the ```AutoValue``` annotation.
