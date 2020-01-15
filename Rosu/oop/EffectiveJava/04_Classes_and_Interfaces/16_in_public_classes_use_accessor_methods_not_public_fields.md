# In public classes, use accessor methods, not public fields

Degenerate classes like this should not be public:

```java
class Point {
    public double x;
    public double y;
}
```

Because data fields of such classes are accessed directly, these classes do not offer the benefits of [_encapsulation_](./15_minimize_the_accessibility_of_classes_and_members.md). You can't change the representation without changing the API, you can't enforce invariants, and you can't take auxiliary action when a field is accessed.
These classes should always be replaced by classes with private fields and public _accessor methods_ (getters) and, for mutable classes, _mutators_ (setters):

```java
class Point {
    private double x;
    private double y;

    public Point(final double x, final double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public void setX(final double x) {
        this.x = x;
    }

    public void setY(final double y) {
        this.y = y;
    }
}
```

**If a class is accessible outside its package, provide accessor methods** to preserve the flexibility to change the class's internal representation.

However, **if a class is package-private or is a private nested class, there is nothing inherently wrong with exposing its data fields** - assuming they do an adequate job of describing the abstraction provided by the class. While the client code is tied to the class's internal representation, this code is confined to the package containing the class.

While it's never a good idea for a public class to expose fields directly, it is less harmful if the fields are immutable. You can't change the representation of such a class without changing its API, and you can't take auxiliarity actions when a field is read, but you can enforce invariants.

```java
public final class Time {
    private static final int HOURS_PER_DAY = 24;
    private static final int MINUTES_PER_HOUR = 60;

    public final int hour;
    public final int minute;

    public Time(final int hour, final int minute) {
        if(hour < 0 || hour >= HOURS_PER_DAY) {
            throw new IllegalArgumentException("Hour: " + hour);
        }
        if(minute < 0 || minute >= MINUTES_PER_HOUR) {
            throw new IllegalArgumentException("Minute: " + minute);
        }
        this.hour = hour;
        this.minute = minute;
    }
}
```
