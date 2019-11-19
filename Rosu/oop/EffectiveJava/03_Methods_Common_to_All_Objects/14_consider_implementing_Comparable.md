# Consider implementing ```Comparable```

The ```compareTo``` method found in the ```Comparable``` interface is similar to the ```Object```'s ```equals``` method, except that it permits order comparison in addition to simple equality comparison, and it is generic.

By implementing ```Comparable```, a class indicates that its instances have a _natural ordering_. Sorting an array of objects that implement ```Comparable``` is as simple as this:

```java
Arrays.sort(a);
```

By implementing ```Comparable```, you allow your class to interoperate with all of the may generic algorithms and collection implementations that depend on this interface. Virtually all of the value classes in the Java platform libraries, as well as [enum types](../06_Enums_and_Annotations/34_use_enums_instead_of_int_constants.md), implements ```Comparable```.

## Generic Contract of the ```compareTo``` method

Returns a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object. Throws ```ClassCastException``` if the specified object's type prevents it from being compared to this object.

* The implementor must ensure that ```sgn(x.compareTo(y)) == -sgn(y.compareTo(x))``` for all the ```x``` and ```y```. This implies that ```x.compareTo(y)``` throws an exception if and only if ```y.compareTo(x)``` throws an exception.
* The implementor must also ensure that the relation is transitive: ```(x.compareTo(y) > 0 && y.compareTo(z) > 0)``` implies that ```x.compareTo(z) > 0```.
* Finally, the implementor must ensure that ```x.compareTo(y) == 0``` implies that ```sgn(x.compareTo(z)) == sgn(y.compareTo(z))```, for all ```z```.
* It is strongly recommended, but not required, that ```(x.compareTo(y) == 0) == x.equals(y)```. Any class that violates this condition should clearly indicate this fact: _"Note: This class has a natural ordering that it is incosistent with ```equals```"._

### ```equals``` similarities

A ```compareTo``` method must obey the same restrictions imposed by the ```equals``` contract: reflexivity, symmetry, and transitivity. Therefore, the same caveat applies: there is no way to extend an instantiable class with a new value component while preserving the ```compareTo``` contract, unless you are willing to forgot the benefits of [object-oriented abstraction](10_obey_the_general_contract_when_overriding_equals.md). The same workarounds apply, too.

Because with the generic interface there is no need for checking the argument type with the operator ```instaceof```, if the argument is ```null```, the invocation should throw ```NullPointerException```, and it will, as soon as the method attempts to access its members.

## Ordering

In a ```compareTo``` method, fields are compared for order rather than equality.

To compare object reference fields, invoke the ```compareTo``` method recursively. If a field does not implement ```Comparable``` or you need a nonstandard ordering, use a ```Comparator``` instead.

```java
public final class CaseInsensitiveString implements Comparable<CaseInsensitiveString> {
    @Override
    public int compareTo(CaseInsensitiveString o) {
        return String.CASE_INSENSITIVE_ORDER.compare(s, o.s);
    }
}
```

In Java 7, static ```compare``` methods were added to all of Java's boxed primitive classes. **Use of the relational operators ```<``` and ```>``` in ```compareTo``` methods is verbose and error-prone and no longer recommended.**

If a class has multiple significant fields, the order in which you compare them is critical. Start with the most significant field and work your way down. If a comparison results in anything other than zero, you're done; just return the result.

```java
@Override
public int compareTo(PhoneNumber p) {
    int result = Short.compare(areaCode, p.areaCode);
    if (result == 0) {
        result = Short.compare(prefix, p.prefix);
        if (result == 0) {
            result = Short.compare(lineNum, p.lineNum);
        }
    }
    return result;
}
```

### ```Comparator``` interface

In Java 8, the ```Comparator``` interface was outfitted with a set of _comparator constructor methods_, which enable fluent construction of comparators.

```java
private final static Comparator<PhoneNumber> COMPARATOR = comparingInt(PhoneNumber::areaCode)
                                                .thenComparingInt(PhoneNumber::prefix)
                                                .thenComparingInt(PhoneNumber::lineNum);

@Override
public int compareTo(PhoneNumber phoneNumber) {
    return COMPARATOR.compare(this, phoneNumber);
}
```

The ```int``` versions of the methods ```comparing``` can also be used for narrower integral types, such as ```sort```. The ```double``` version can also be used for ```float```. This provides coverage of all Java's numerical primitive types.

There are also comparator construction methods for object reference types. The static method ```comparing``` has two overloadings:

* One takes a key extractor, uses key's natural order.
* The second takes both a key extractor and a comparator to be used on the extracted keys.

There are three overloadings of the instance method, ```thenComparing```:

* One takes only a comparator and uses it to provide a secondary order.
* The second takes only a key extractor and uses the key's natural order.
* The third one takes the key extractor and a comparator to be used on the extracted keys.

### Taking advantage of the method ```hashCode```

You may see ```compareTo``` or ```compare``` methods that rely on the fact that the difference between two values is negative if the first value is less than the second, zero if the two values equal, and positive if the first value is greater.

```java
// Broken! Violates transitivity!
static Comparator<Object> hashCodeOrder = new Comparator<>() {
    @Override
    public int compare(final Object o1, final Object o2) {
        return o1.hashCode() - o2.hashCode();
    }
}
```

Do not use this technique. It is fraught with danger from integer overflow and IEEE 754 floating point arithmetic artifacts. Use either a static ```compare``` method:

```java
static Comparator<Object> hashCodeOrder = new Comparator<>() {
    @Override
    public int compare(final Object o1, final Object o2) {
        return Integer.compare(o1.hashCode(), o2.hashCode());
    }
}
```

or a comparator construction method:

```java
static Comparator<Object> hashCodeOrder = Comparator.comparingInt(Object::hashCode);
```
