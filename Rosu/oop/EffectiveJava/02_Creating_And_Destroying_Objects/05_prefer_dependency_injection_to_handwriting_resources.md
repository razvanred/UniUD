# Prefer dependency injection to handwriting resources

Many classes depend on one or more underlying resources. For example, a spell checker depends on a dictionary.

It is not uncommon to see such classes implemented as [static utility classes](04_enforce_noninstantiability_with_a_private_constructor.md):

```java
// Inappropriate use of static utility - inflexible & untestable
public class SpellChecker {
    private static final Lexicon dictionary = new ItalianLexicon();

    private SpellChecker() { // Noninstantiable
        throw AssertionError();
    }

    public static boolean isValid(final String word) { }
    public static List<String> suggestions(final String typo) { }
}
```

It's not uncommon to see them implemented as [singletons](03_enforce_the_singleton_property_with_a_private_constructor_or_an_enum_type.md):

```java
// Inappropriate use of singleton - inflexible & untestable
public class SpellChecker {
    private final Lexicon dictionary = new ItalianLexicon();
    public static final SpellChecker INSTANCE = new SpellChecker();

    private SpellChecker() { } // Instantiable just by the static object contained here

    public static boolean isValid(final String word) { }
    public static List<String> suggestions(final String typo) { }
}
```

Neither of this approaches is satisfactory, because they assume that there is only one dictionary worth using: it may be desiderable to use a special dictionary for testing.
You could try to have ```SpellChecker``` support multiple dictionaries by making the ```dictionary``` field nonfinal and adding a method to change the dictionary in an existing spell checker, but this would be awkward, error-prone, and unworkable in a concurrent setting. <mark style="background-color: lightgreen">Static utility classes and singletons are inappropriate for classes whose behavior is parameterized by an underlying resource.</mark>

What is required is the ability to support multiple instances of the class, each of which uses the resource desired by the client. A simple pattern that satisfies this requirement is to <mark style="background-color: lightgreen">pass the resource into the constructor when creating a new instance.</mark> This is one form of **dependency injection**: the dictionary is a _dependency_ of the spell checker and is _injected_ into the spell checker when is created.

```java
// Dependency Injection provides flexibility and testability
public class SpellChecker {
    private final Lexicon dictionary;

    public SpellChecker(final Lexicon dictionary) {
        this.dictionary = dictionary;
    }

    public static boolean isValid(final String word) { }
    public static List<String> suggestions(final String typo) { }
}
```

The dependency injection pattern:

* it works with an arbitrary number of resources and arbitrary dependency graphs;
* it preserves [immutability](../04_Classes_and_Interfaces/17_minimize_mutability.md), so multiple clients can share dependent objects (assuming the clients desire the same underlying resources). Dependency injection is equally applicable to constructors, [static factories](01_static_factory_methods.md), and [builders](02_Creating_And_Destroying_Objects\02_consider_a_builder_when_faced_with_may_constructor_parameters.md).

A useful variant of the pattern is to pass a resource _factory_ to the constructor. A factory is an object that can be called repeatedly to create instances of a type. Such factories enbody the _Factory Method_ pattern. The ```Supplier<T>``` interface, introduced in Java 8, is perfect for representing factories. Methods that take a ```Supplier<T>``` on input should typically constrain the factory's type parameter using a [bounded wildcard type](../05_Generics/31_use_bounded_wildcards_to_increase_API_flexibility.md) to allow the client to pass in a factory that creates any subtype of a specified type.

```java
Mosaic create(final Supplier<? extends Tile> tileFactory) { }
```

Althrough dependency injection greately improves flexibility and testability, it can clutter up large projects, which typically contain thousands of dependencies. This clutter can be all but eliminated by using a _dependency injection framework_, such as [Dagger](http://dagger.dev/), Guice or Spring.
