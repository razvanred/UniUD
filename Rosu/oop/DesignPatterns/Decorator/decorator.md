# Decorator Pattern

Decorator pattern allows a user to add a new functionaly to an existing object without altering its structure. This type of design pattern comes under structural pattern as **this pattern acts as a wrapper to an existing class**.

This pattern creates a decorator class which wraps the original class and provides additional functionality keeping class methods signature intact.

## Implementation

![alt text](decorator_pattern_uml_diagram.jpg "Decorator Pattern UML Diagram")

### Step 1: Create the interface

```java
public interface Shape {
    void draw();
}
```

### Step 2: Create concrete class implementations

```Rectangle.java```

```java
public class Rectangle implements Shape {
    @Override
    public void draw() {
        System.out.println("Shape: Rectangle");
    }
}
```

```Circle.java```

```java
public class Circle implements Shape {
    @Override
    public void draw() {
        System.out.println("Shape: Circle");
    }
}
```

### Step 3: Create abstract decorator implementing the ```Shape``` interface

```java
public abstract class ShapeDecorator implements Shape {
    protected final Shape decoratedShape;

    public ShapeDecorator(final Shape decoratedShape) {
        this.decoratedShape = decoratedShape;
    }

    @Override
    public void draw() {
        decoratedShape.draw();
    }
}
```

### Step 5: Use the ```RedShapeDecorator``` to decorate ```Shape``` objects

```java
public class Principale {
    private Principale() {
        throw new AssertionError();
    }

    public static void main(String[] args) {
        final Shape circle = new Circle();
        final Shape redCircle = new RedShapeDecorator(new Circle());
        final Shape redRectangle = new RedShapeDecorator(new Rectangle());

        System.out.println("Circle with normal border");
        circle.draw();

        System.out.println("\nCircle with red border");
        redCircle.draw();

        System.out.println("\nRectangle with red border");
        redRectangle.draw();
    }
}
```

### Step 6: Verify the output

```txt
Circle with normal border
Shape: Circle

Circle with red border
Shape: Circle
Border color: Red

Rectangle of red border
Shape: Rectangle
Border Color: Red
```
