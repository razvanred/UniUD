# Always override ```hashCode``` when you override ```equals```

**You must override ```hashCode``` in every class that overrides ```equals```.** If you fail to do so, your class will violate the general contract for ```hashCode```, which will prevent it from functioning properly in collections such as ```HashMap``` and ```HashSet```.

* When the ```hashCode``` method is invoked on an object repeatedly during an execution of an application, it must consistently return the same value, provided no information used in ```equals``` comparison is modified. This value need not remain consistent from one execution of an application to another.
* If two objects are equal according to the ```equals``` method, then calling ```hashCode``` on the two objects must produce the same integer result.
* If two objects are unequal according to the ```equals``` method, it is not required that calling ```hashCode``` on each of the objects must produce distinct results. However, the programmer should be aware that producing distinct results for unequal objects may improve the performance of hash tables.

**The key provision that is violated when you fail to override ```hashCode``` is the second one: equal objects must have equal hash codes.**

## Implementation

A good hash function tends to produce unequal hash codes for unequal instances. Ideally, a hash function should distribute any reasonable collection of unequal instances uniformly across all ```int``` values. Here's how you can achieve a fair approximation:

* Declare an ```int``` variable named ```result```, and initialize it to the hash code ```c``` for the first significant field in your object.
* For every significant field ```f```, do the following:
  * Compute an ```int``` hash code ```c``` for the field:
    * If the field is of a primitive type, compute ```Type.hashCode(f)```, where ```Type``` is the boxed primitive class corresponding to ```f```'s type.
    * If the field is an object reference and this class's ```equals``` method compares the field by recursively invoking ```equals```, recursively invoke ```hashCode``` on the field. If a more complex comparison is required, compute a "canonical representation" for this field and invoke ```hashCode``` on the canonical representation. If the value of the field is ```null```, use ```0``` (or some other constant, but ```0``` is traditional).
    * If the field is an array, treat it as if each significant element were a separate field. That is, compute a hash code for each significant element by applying these rules recursively, and combine the values per the next step. If the array has no significant elements, use a constant, prefereably not ```0```. If all elements are significant, use ```Arrays.hashCode```.
  * Combine the hash code ```c``` computed in the last step into ```result``` as follows:

    ```java
    result = 31 * result + c;
    ```

* Return ```result```.

You may exclude _derived fields_ from the hash code computation. You _must_ exclude any fields that are not used in ```equals``` comparison, or you risk violating the second provision of the ```hashCode``` contract.

### Implementation example

#### Traditional implementation

```java
@Override
public int hashCode() {
    int result = Short.hashCode(areaCode);
    result = 31 * result + Short.hashCode(prefix);
    result = 31 * result + Short.hashCode(lineNum);
    return result;
}
```

#### Adviced implementation (Java 7 required)

This implementation has mediocre performance, compared to the traditional implementation. This approach is recommended for use only in situations where performance is not critical.

```java
@Override
public int hashCode() {
    return Objects.hash(areaCode, prefix, lineNum);
}
```

### Caching the hash

If a class is immutable and the cost of computing the hash is significant, you might consider caching the hash code in the object rather than recalculating it each time it is requested.

```java
public class Car {
    private final String brand;
    private final String model;
    private final int yearOfProduction;

    private int hashCode; // Automatically initialized to 0

    public Car(final String brand, final String model, final int yearOfProduction) {
        this.brand = brand;
        this.model = model;
        this.yearOfProduction = yearOfProduction;
    }

    @Override
    public boolean equals(final Object other) {
        if(!(other instanceof Car)) {
            return false;
        }
        final var car = (Car) other;
        return car.brand.equals(brand) && model.equals(car.model) && yearOfProduction == car.yearOfProduction;
    }

    @Override
    public int hashCode() {
        int result = hashCode;
        if(result == 0) {
            result = brand.hashCode();
            result = 31 * result + model.hashCode();
            result = 31 * result + Integer.hashCode(yearOfProduction);
            hashCode = result;
        }
        return result;
    }
}
```

## Advices

* **Do not be tempted to exclude significant fields from the hash code computation to improve performance.**
* **Don't provide a detailed specification for the value returned by ```hashCode```, so clients can't reasonably depend on it; this gives you the flexibility to change it.**
