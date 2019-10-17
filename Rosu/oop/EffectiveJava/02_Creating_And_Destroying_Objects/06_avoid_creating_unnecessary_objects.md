# Avoid creating unnecessary objects

An object can always be reused if it is [immutable](../04_Cleasses_and_Interfaces/17_minimize_mutability.md).

```java
final String s = new String("bikini");
```

This statement creates a new ```String``` instance each time it is executed, and none of these object creations is nencessary. The argument to the ```String``` constructor ```("bikini")``` is itself a ```String``` instance, functionally identical to all of the objects created by the constructor.

Improved version:

```java
final String s = "bikini";
```

It is guaranteed that the object will be reused by any other code running in the same virtual machine that happens to contain the same string literal [JSL, 3.10.5](https://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.5).

You can often avoid creating unnecessary objects by using [static factory methods](01_static_factory_methods.md): the factory method ```Boolean.valueOf(String)``` is preferable to the constructor ```Boolean(String)```, which was deprecated in Java 9. The constructor _must_ create a new object each time it's called, while the factory method is never required to do so and won't in practice.

## Cache expensive objects

```java
// Performance can be greately improved!
static boolean isRomanNumeral(final String s) {
    return s.matches("[roman_regex]");
}
```

<mark style="background-color: lightgreen">While ```String.matches``` is the easiest way to check if a string matches a regular expression, it's not suitable for repeated use in performance-critical situations.</mark> The problem is that internally creates a ```Pattern``` instance for the regular expression and uses it only once, after which it becomes eligible for garbage collection. Creating a ```Pattern``` instance is expensive because it requires compiling the regular expression into a finite state machine.

```java
// Performance improved: reusing expensive object
public class RomanNumerals {
    private static final Pattern ROMAN = Pattern.compile("[roman_regex]");

    static boolean isRomanNumeral(final String s) {
        return ROMAN.matcher(s).matches();
    }
}
```

## Autoboxing

<mark style="background-color: lightgreen">Autoboxing blurs but does not erase the distinction between primitive and boxed primitive types.</mark> There are subtle semantics dictions and not-so-subtle performance differences:

```java
// Slow, spot the object creation!
private static long sum() {
    Long sum = 0L;
    for(long i = 0; i <= Integer.MAX_VALUE; i++) {
        sum += i;
    }
    return sum;
}
```

The variable ```sum``` is declared as a ```Long``` instead of a ```long```, which means that the program constructs 2^31 unnecessary ```Long``` instances (one for each time the ```long``` is added to the ```Long``` sum).

<mark style="background-color: lightgreen">Prefer primitive to boxed primitives, and watch out for unintentional autoboxing.</mark>

## Avoid Object pools

This item should not me misconstructed to imply that object creation is expensive and should be avoided. Creating additional objects to enhance the clarity, simplicity, or power of a program is generally a good thing.

Conversely, avoiding object creation by maintaining your own _object pool_ is a bad idea unless the objects in the pool are extremely heavyweight (like a _database connection_, where the cost of establishing the connection is sufficiently high that it makes sense to reuse these objects).

<mark style="background-color: #ff668c">Failing to make defensive copies where required can lead to insidious bugs and security holes; creating objects unnecessarily merely affects style and performance.</mark>
