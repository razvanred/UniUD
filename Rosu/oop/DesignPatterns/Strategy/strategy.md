# Strategy Pattern

The strategy patthern (also known as the **policy pattern**) is a _behavioral software design pattern_ that enables selecting an algorithm at runtime. Instad of implementing a single algorithm directly, code receives run-time instructions as to which in a family of algorithms to use.

Stategy lets the algorithm very indipendently from clients that use it. The validation algorithms (strategies), encapsulated separately from the validating object, may be used by other validating objects in different areas of the system (or even in different systems) without code duplication.

## Structure

![alt text](strategy_pattern_uml_example.jpg "Strategy Pattern UML Diagram")

In the above UML diagram, the ```Context``` class doesn't implement an algorithm directly. Instad, ```Context``` refers to the ```Strategy``` interface for performing an algorithm, ```strategy.algoritm()```, which makes ```Context``` indipendent of how an algorithm is implemented. The ```Strategy1``` and ```Strategy2``` classes implement the ```Strategy``` interface, that is, implement (encapsulate) an algorithm.

This diagram shows also the run-time interactions: the ```Context``` object delegates an algoritm to different ```Strategy``` objects. First, ```Context``` calls ```algorithm()``` on a ```Strategy1``` object, which performs the algorithm and returns the result to ```Context```. Thereafetr, ```Context``` changes its strategy and calls ```algorithm()``` on a ```Strategy2``` object, which performs the algorithm and returns the result to ```Context```.

## Example

You can try the examples written in [Swift](./billing_strategy.swift) and [Java](Strategy.java).

## Strategy and open/closed principle

According to the strategy pattern, the behaviors of a class should not be inherited. Instead they should be encapsulated using interfaces. This is compatible with the open/closed principel, which proposes that classes should be open for extension but closed for modification.

As an example, consider a car class. Two possible functionalities for car are *brake* and *accelerate*. Since accelerate and brake behaviors change frequently between models, a common approach is to implement these behaviours in subclasses. This approach has significant drawbacksç accelerate and brave behaviors must be declared in each new Car model. The work of managing these behaviors increases greatly as the numer of models increases, and requires code to be duplicated across models. Additionally, it is not easy to determine the exact nature of the behavior for each model without investigating the code in each.

The strategy pattern uses [composition instead of inheritance](../../EffectiveJava/04_Classes_and_Interfaces/18_favor_composition_over_inheritance.md). In this strategy pattern, behaviors are defined as separated interfaces and specific classes that implement these interfaces. This allows better decoupling between the behavior and the class that uses the behavior.

```java
interface BrakeBehavior {
    void brake();
}

class BrakeWithABS implements BrakeBehavior {
    @Override
    public void brake() {
        System.out.println("Brake with ABS applied");
    }
}

class Brake implements BrakeBehavior {
    @Override
    public void brake() {
        System.out.println("Simple Brake applied");
    }
}

abstract class Car {
    private BrakeBehaviour brakeBehavior;

    public Car(final BrakeBehavior brakeBehavior) {
        this.brakeBehavior = brakeBehavior;
    }

    public void applyBrake() {
        brakeBehavior.brake();
    }

    public void setBrakeBehavior(final BrakeBehavior brakeBehavior) {
        this.brakeBehavior = brakeBehavior;
    }
}

public class Sedan extends Car {
    public Sedan() {
        super(new Brake());
    }
}

public class SUV extends Car {
    public SUV() {
        super(new BrakeWithABS());
    }
}

public class CarExample {
    private CarExample() {
        throw new AssertionError();
    }

    public static void main(final String[] args) {
        final Car sedanCar = new SedanCar();
        final Car suvCar = new SUV();

        sedanCar.applyBrake();
        suvCar.applyBrake();

        suvCar.setBrakeBehavior(new Brake());
        suvCar.applyBrake();
    }
}
```
