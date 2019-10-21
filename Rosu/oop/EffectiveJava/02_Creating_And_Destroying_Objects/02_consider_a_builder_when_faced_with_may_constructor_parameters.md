# Consider a Builder when faced with many constructor parameters

Static factories and constructors share a limitation: they do not scale well to large numbers of optional parameters.

## Telescoping Constructor Pattern

Traditionally, programmers have used the _telescoping constructor_ pattern, in which you provide a constructor with only the requested parameters, another with a single optional parameter, a third with two optional paramters, and so on, culminating in a constructor with all the optional parameters.

Example:

```java
public class NutritionFacts {
    private final int servingSize;  // mL            (required)
    private final int servings;     // per container (required)
    private final int calories;     // per servings  (optional)
    private final int fat;          // g/serving     (optional)
    private final int sodium;       // mg/serving    (optional)
    private final int carbohydrate; // g/serving     (optional)

    public NutritionFacts(
        final int servingSize,
        final int servings
    ) {
        this(servingSize, servings, 0);
    }

    public NutritionFacts(
        final int servingSize,
        final int servings,
        final int calories
    ) {
        this(servingSize, servings, calories, 0);
    }

    public NutritionFacts(
        final int servingSize,
        final int servings,
        final int calories,
        final int fat
    ) {
        this(servingSize, servings, calories, fat, 0);
    }

    public NutritionFacts(
        final int servingSize,
        final int servings,
        final int calories,
        final int fat,
        final int sodium
    ) {
        this(servingSize, servings, calories, fat, sodium, 0);
    }

    public NutritionFacts(
        final int servingSize,
        final int servings,
        final int calories,
        final int fat,
        final int sodium,
        final int carbohydrate
    ) {
        this.servingSize = servingSize;
        this.servings = servings;
        this.calories = calories;
        this.fat = fat;
        this.sodium = sodium;
        this.carbohydrate = carbohydrate;
    }
}
```

When you want to create an instance, you use the constructor with the shortest parameter list containing all the parameters you want to set:

```java
final NutritionFacts cocaCola = new NutritionFacts(240, 8, 100, 0, 35, 27);
```

Typically this constructor invocation will require many parameters that you don't want to set, but you're forced to pass a value for them anyway.

<mark>The telescoping constructor pattern works, but it is hard to write client code when there are many parameters, and harder, still to read it.</mark>

## JavaBean Pattern

In this pattern you call a parameterless constructor to create the object and then call setter methods to set each required parameter and each optional paramter of interest:

```java
public class NutritionFacts {
    private int servingSize = -1 // Required; no default value
    private int servings = -1;   // Required; no default value
    private int calories = 0;
    private int fat = 0;
    private int sodium = 0;
    private int carbohydrate = 0;

    public NutritionFacts() { }

    public void setServingSize(int servingSize) {
        this.servingSize = servingSize;
    }

    public void setServings(int servings) {
        this.servings = servings;
    }

    public void setCalories(int calories) {
        this.calories = calories;
    }

    public void setFat(int fat) {
        this.fat = fat;
    }

    public void setSodium(int sodium) {
        this.sodium = sodium;
    }

    public void setCarbohydrate(int carbohydrate) {
        this.carbohydrate = carbohydrate;
    }
}
```

The patten has none of the disadvantages of the telescoping constructor pattern. It is easy to create instances and to read the resulting code:

```java
final NutritionFacts cocaCola = new NutritionFacts();
cocaCola.setServingSize(240);
cocaCola.setServings(8);
cocaCola.setCalories(100);
cocaCola.setSodium(35);
cocaCola.setCarbohydrate(27);
```

Unfortunately, the JavaBeans pattern has serious disadvantages of its own:

* because construction is split across multiple calls, <mark>a JavaBean may be in an inconsistent state partway through its construction</mark>;
* <mark>it precludes the possibility of making a class [immutable](../04_Cleasses_and_Interfaces/17_minimize_mutability.md)<mark> and requires added effort on the part of the programmer to ensure thead safety.

It is possible to reduce these disadvantages by manually freezing the object when its construction is complete and not allowing it to be used until frozen, but this varian is unwieldy and rarely used in practice.

## Builder Pattern

It combines the safety of the telescoping constructor pattern with the readibility of the JavaBeans pattern.

Instead of making the desired object directly, the client calls a constructor (or a static factory) with all of the required parameters and gets a _builder object_.

Typically, the builder is a [static member class](../04_Classes_and_Interfaces/24_favor_static_member_classes_over_nonstatic.md) of the class it builds.

```java
public class NutritionFacts {
    private final int servingSize;
    private final int servings;
    private final int calories;
    private final int fat;
    private final int sodium;
    private final int carbohydrate;

    public static class Builder {
        // Required parameters
        private final int servingSize;
        private final int servings;

        //Optional parameters
        private int calories = 0;
        private int fat = 0;
        private int sodium = 0;
        private int carbohydrate = 0;

        public Builder(
            final int servingSize,
            final int servings
        ) {
            this.servingSize = servingSize;
            this.servings = servings;
        }

        public Builder calories(int calories) {
            this.calories = calories;
            return this;
        }

        public Builder fat(int fat) {
            this.fat = fat;
            return this;
        }

        public Builder sodium(int sodium) {
            this.sodium = sodium;
            return this;
        }

        public Builder carbohydrate(int carbohydrate) {
            this.carbohydrate = carbohydrate;
            return this;
        }

        public NutritionFacts build() {
            return new NutritionFacts(this);
        }
    }

    private NutritionFacts(
        final Builder builder
    ) {
        servingSize = builder.servingSize;
        servings = builder.servings;
        calories = builder.calories;
        fat = builder.fat;
        sodium = builder.sodium;
        carbohydrate = builder.carbohydrate;
    }
}
```

The ```NutritionFacts``` class is immutable, and all parameter default values are in one place. The builder's setter methods return the builder itself so that the invocations can be chained, resulting in a _fluent_ API.

Client code:

```java
final NutritionFacts cocaCola = new Builder(240, 80)
                .calories(100)
                .sodium(35)
                .carbohydrate(27)
                .build();
```

### Invalid parameters

To detect invalid parameters as soon as possible:

* Check parameter validity in the builder's constructor and methods.
* Check invariants involving multiple parameters in the constructor invoked by the ```build``` method:
  1. To ensure these invariants against [attack](../08_Methods/50_make_defensive_copies_when_needed.md), do the checks on object fields after copying parameters from the builder.
  2. If a check fails, throw an [IllegalArgumentException](../10_Exceptions/72_favor_the_use_of_standard_exceptions.md) whose [detail message](../10_Exceptions/75_include_failure-capture_information_in_detail_messages.md) indicates which parameters are invalid.

### Class Hierarchies

Use a parallel hierarchy of builders, each nested in the corresponding class. <mark>Abstract classes have abstract builders; concrete classes have concrete builders.</mark>

Consider an abstract class at the root of hierarchy representing various kinds of pizza:

```java
public abstract class Pizza {
    public enum Topping {
        HAM,
        MUSHROOM,
        ONION,
        PEPPER,
        SAUSAGE
    }
    final Set<Topping> toppings;

    abstract static class Builder<T extends Builder<T>> {
        protected EnumSet<Topping> toppings = EnumSet.noneOf(Topping.class);
        public T addTopping(final Topping topping) {
            toppings.add(Object.requireNotNull(topping));
            return self();
        }

        abstract Pizza build();

        protected abstract T self();
    }

    Pizza(final Builder<?> builder) {
        toppings = builder.toppings.clone(); // See Item 50
    }
}
```

Note that ```Pizza.Builder``` is a _generic type_ with a [recursive type parameter](../05_Generics/30_favor_generic_methods.md). This, along with the abstract ```self``` method, allows method chaining to work properly in subclasses, without the need for casts. This workaround for the fact that Java lacks a self type is known as **simulated self-type** idiom.

Concrete Pizza classes:

```java
public class NyPizza extends Pizza {
    public enum Size {
        SMALL,
        MEDIUM,
        LARGE
    }

    private final Size size;

    public static class Builder extends Pizza.Builder<Builder> {
        // required parameter
        private final Size size;

        public Builder(final Size size) {
            this.size = Objects.requireNotNull(size);
        }

        @Override
        public NyPizza build() {
            return new NyPizza(this);
        }

        @Override
        protected Builder self() {
            return this;
        }
    }

    private NyPizza(final Builder builder) {
        super(builder);
        size = builder.size;
    }
}

public class Calzone extends Pizza {
    private final boolean sauceInside;

    public static class Builder extends Pizza.Builder<Builder> {
        // here you should specify whether the sauce should be inside or out
        private boolean sauceInside = false;
    }

    public Builder sauceInside() {
        sauceInside = true;
        return this;
    }

    @Override
    public Calzone build() {
        return new Calzone(this);
    }

    @Override
    protected Builder self() {
        return this;
    }

    private Calzone(final Builder builder) {
        super(builder);
        sauceInside = builder.sauceInside;
    }
}
```

The ```build``` method in each subclass's builder is declared to return the correct subclass. This technique, wherein a subclass method is declared to return a subtype of the return type declared in the super class, is known as **covariant return typing**. It allows clients to use these builders without the need for casting.

```java
final NyPizza pizza = NyPizza.Builder(SMALL)
                        .addTopping(SAUSAGE)
                        .addTopping(ONION)
                        .build();

final Calzone calzone = Calzone.Builder()
                        .addTopping(HAM)
                        .sauceInside()
                        .build();
```

### Advantages

* Unlike constructors, builders can have multiple varargs parameters because each parameter is specified in its own method;
* builders can aggregate the parameters passed into multiple calls to a method into a single field (the ```àddTopping``` method);
* a single builder can be used repeatedly to build multiple objects;
* a builder can fill in some fields automatically upon object creation, such as serial number that increases each time the object is created.

### Disadvantages

* In order to create an object, you must first create its builder: it could be a problem in performance-critical situations.
* The Builder pattern is more verbose than the telescoping constructor pattern, so it should be used only if there are enough parameters to make it worthwhile (say 4 or more).
