# Override ```clone``` judiciously

The ```Cloneable``` interface was intended to be a [mixin interface](../04_Classes_and_Interfaces/20_prefer_interfaces_to_abstract_classes.md) for classes to advertise that they permit cloning. Unfortunately, it fails to serve this purpose. Its primary flaw is that it lacks a ```clone``` method, and ```Object```'s ```clone``` method is protected. You cannot, without resorting to [reflection](../09_General_Programming/65_prefer_interfaces_to_reflection.md), invoke ```clone``` on an object merely because it implements ```Cloneable```. Even a reflective invocation may fail, because there is no guarantee that the object has an accessible ```clone``` method.

The ```Cloneable``` interface determines the behavior of ```Object```'s protected ```clone``` implementation: if a class implements ```Cloneable```, ```Object```'s ```clone``` method returns a field-by-field copy of the objet; otherwise it throws ```CloneNotSupportedException```.

Normally, implementing an interface says something about what a class can do for its clients. In this case, it modifies the behavior of a protected method on a superclass.

**In practice, a class implementing ```Cloneable``` is expected to provide a properly functioning public ```clone``` method.** The mechanism is fragile, dangerous and _extralinguistic_: it creates objects without calling a constructor.

## General Contract

For any object ```x```, the expression:

```java
x.clone() != x
```

will be ```true```, and the expression

```java
x.clone().getClass() == x.getClass()
```

will be ```true```, but these are not absolute requirements. While it is typically the case that

```java
x.clone().equals(x)
```

will be ```true```, this is not an absolute requirement.

## Flaws

If a class's ```clone``` method returns an instance that is not obtained by calling ```super.clone``` but by calling a constructor, the compiler won't complain, but if a subclass of that class calls ```super.clone```, the resulting object will have the wrong class, preventing the subclass from ```clone``` method from working properly.

If a class that overrides ```clone``` is final, this convention, this convention may be safely ignored. But if a final class has a ```final``` method that does not invoke ```super.clone```, there is no reason for the class to implement ```Cloneable```, as it doesn't rely on the behavior of ```Object```'s clone implementation.

Note that **immutable classes should never provide a ```clone``` method** because it would merely encourage wasteful copying.

Here's how a ```clone``` method for ```PhoneNumber``` would look like:

## Implementation example

```java
@Override
public PhoneNumber clone() {
    try {
        return (PhoneNumber) super.clone();
    } catch (final CloneNotSupportedException e) {
        throw new AssertionError();
    }
}
```

Java supports _covariant return types_: an overloading method's return type can be a subclass of the overriden method's return type. The cast is guaranteed to succeed.

Object declares its ```clone``` method to throw ```CloneNotSupportedException``` which is a _checked exception_. The need for this boilerplate indicates that ```CloneNotSupportedException``` should have been [unchecked](../10_Exceptions/71_avoid_unnecessary_use_of_checked_exceptions.md).

## Mutable objects

If an object contains fields that refer to mutable objects, the simple ```clone``` implementation shown earlier can be disastrous.

```java
public class Stack {
    private final static int DEFAULT_INITIAL_CAPACITY = 16;

    private Objects[] elements;
    private int size;

    public Stack() {
        elements = new Object[DEFAULT_INITIAL_CAPACITY];
    }

    public void push(final Object element) {
        ensureCapacity();
        elements[size++] = element;
    }

    public Object pop() {
        if(size == 0) {
            throw new EmptyStackException();
        }
        final Object result = elements[--size];
        elements[size] = null; // Eliminate obsolete reference
        return result;
    }

    private void ensureCapacity() {
        if(size == elements.length) {
            elements = Arrays.copyOf(elements, size * 2 + 1);
        }
    }
}
```

Suppose you want to make this class cloneable. If the ```clone``` method merely returns ```super.clone()```, the resulting ```Stack``` instance will have the correct value in its ```size``` field, but its ```elements``` field will refer to the same array as the original ```Stack``` instance. Modifying the original will destroy the invariants in the clone and vice versa.

**In effect, the ```clone``` method functions as a constructor; you must ensure that it does no harm to the original object and that it properly establishes invariants on the clone**. In order for the ```clone``` method on ```Stack``` to work properly, it must copy the internals of the stack. The easiest way to do this is to call ```clone``` recursively on the ```elements``` array:

```java
@Override
public Stack clone() {
    try {
        final Stack result = (super) super.clone();
        result.elements = elements.clone(); // Preferred idiom to duplicate an array
        return result;
    } catch (final CloneNotSupportedException e) {
        throw new AssertionError();
    }
}
```

This solution would not work if the ```elements``` field were final becase ```clone``` would be prohibited from assigning a new value to the field.

Like serialization, **the ```Cloneable``` architecture is incompatible with normal use of final fields referring to mutable objects**, except in cases where the mutable objects may be safely shared between an obejct and its clone. In order to make a class cloneable, it may be necessary to remove ```final``` modifiers from some fields.

### Deep Copy

```java
class HashTable implements Cloneable {

    private Entry[] buckets;

    private static class Entry {
        final Object key;
        Object value;
        Entry next;

        Entry(final Object key, final Object value, final Entry next) {
            this.key = key;
            this.value = value;
            this.next = next;
        }
    }
}
```

Suppose you merely clone the bucket array recursively, as we did for ```Stack```:

```java
// Broken - results in shared mutable state!
@Override
public HashTable clone() {
    try {
        final HashTable result = (HashTable) super.clone();
        result.buckets = buckets.clone();
        return result;
    } catch (final CloneNotSupportedException e) {
        throw new AssertionError();
    }
}
```

Through the clone has its own bucket array, this array references the same linked lists as the original, which can easily cause nondeterministic behavior in both the clone and the original. To fix this problem, you'll have to copy the linked list that comprises each bucket.

```java
class HashTable implements Cloneable {

    private Entry[] buckets;

    private static class Entry {
        final Object key;
        Object value;
        Entry next;

        Entry(final Object key, final Object value, final Entry next) {
            this.key = key;
            this.value = value;
            this.next = next;
        }

        Entry deepCopy() {
            final var result = new Entry(key, value, next);
            for (final Entry p = result; p.next != null; p = p.next) {
                p.next = new Entry(p.next.key, p.next.value, p.next.next);
            }
            return result;
        }
    }

    @Override
    public HashTable clone() {
        try {
            final var result = (HashTable) super.clone();
            for (int i = 0; i < buckets.length; i++) {
                if(buckets[i] != null) {
                    result.buckets[i] = buckets[i].deepCopy();
                }
            }
            return result;
        } catch (final CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
```

## Thread-safety

If you write a thread-safe class that implements ```Cloneable```, remember that its ```clone``` method must be properly synchronized, so even if its implementation is otherwise satisfactory, you may have to write a synchronized ```clone``` method that returns ```super.clone()```.

## Copy factory

A better approach to object copying is to provide a _copy constructor_:

```java
public Yum(final Yum yum) {
    this.name = yum.name;
}
```

Or a _copy factory_:

```java
public static Yum newInstance(final Yum yum) {
    return new Yum(yum.name);
}
```

These alternatives to the ```clone``` method can take an argument whose type is an interface implemented by the class. Interface-based copy constructors and factories, more properly known as _conversion constructors_, allow the client to choose the implementation type of the opy rather than forcing the client to accept the implementation type of the orginal.
