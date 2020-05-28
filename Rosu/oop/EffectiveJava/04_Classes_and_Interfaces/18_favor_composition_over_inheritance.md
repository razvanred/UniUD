# Favor composition over inheritance

It is safe to use inheritance within a package, where the subclass and the superclass implementations are under the control of the same programmers. It is also safe to use inheritance when extending [classes specifically designed and documented for extension](19_design_and_document_for_inheritance_or_else_prohibit_it.md).

The problems discussed in this item do not apply to _interface inheritance_ (when a class implements an interface or when an interface extends another).

**Unlike method invocation, inheritance violates encapsulation**. In other words, a subclass depends on the implementation details of its subclass for its proper function. The superclass's implementation may change from release to release, and if it does, the subclass may break, even though its code has not been touched. As a consequence, a subclass must evolve in tandem with its superclass, unless the superclass's authors have designed and documented it specifically for the purpose of being extended.

## ```InstrumentedHashMap``` example

Let's suppose we have a program that uses a ```HashSet```.

To tune the performance of our program, we need to query the ```HashSet``` as to how many elements have been added since it was created (not to be confused with its current size, which goes down when an element is removed). To provide this functionality, we write a ```HashSet``` variant that keeps count of the number of attempted element insertions and exports an accessor for this count. The ```HashSet``` class contains two methods capable of adding elements, ```add``` and ```addAll```, so we override both of these methods:

```java
// Broken! Inappropriate use of inheritance
public class InstrumentedHashSet<E> extends HashSet<E> {
    private int addCount = 0;

    public InstrumentedHashSet() {
    }

    public InstrumentedHashSet(int initCap, float loadFactor) {
        super(initCap, loadFactor);
    }

    @Override
    public boolean add(T t) {
        addCount++;
        return super.add(t);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        addCount += c.size();
        return super.addAll(c);
    }

    public int getAddCount() {
        return addCount;
    }
}
```

This class looks reasonable, but it doesn't work. Suppose we create an instance and add three elements using the ```addAll``` method.

```java
final var hashSet = new InstrumentalHashSet<String>();
hashSet.addAll(List.of("hello", "world", "man"));
```

We would expect the ```getAddCount``` method to return three at this point, but it returns six. What went wrong?

Internally, ```HashSet```'s ```addAll``` method is implemented on top of its ```add``` method, although ```HashSet```, quite reasonably, does not document this implementation detail.

We could fix the subclass by eliminating its override of the ```addAll``` method. While the resulting class would work, it would depend for its proper function on the fact that ```HashSet```'s ```addAll``` method is implemented on top of its ```add``` method. The "self-use" is an implementation detail, not guaranteed to hold in all implementations of the Java platform and subject to change from release to release. Therefore, the resulting ```InstrumentedHashSet``` class would be fragile.

It would slightly better to override the ```addAll``` method to iterate over the specified collection, calling the ```add``` method once for each element. This technique, however, does not solve all our problems. It amounts to reimplementing superclass methods that may or may not result in self-use, which is difficult, time-consuming, error-prone, and may reduce performance.

## Security concerns about implementation inheritance

A related cause of fragility in subclasses is that their superclass can acquire new methods in subsequent releases. Suppose a program depends for its security on the fact that all elements inserted into some collection satisfy some predicate. This can be guaranteed by subclassing the collection and overriding each method capable of adding an element to ensure that the predicate is satisfied before adding the element. This works fine until a new method capable of inserting an element is added to the superclass in a subsequent release. Once this happens, it becomes possible to add an "illegal" element merely by invoking the new method which is not overriden in the subclass.

This is not purely a theoretical problem. Several security holes of this nature had to be fixed when ```Hashtable``` and ```Vector``` were retrofitted to participate in the Collections Framework.

## What if we avoid overriding methods from the superclass

You might think that it is safe to extend a class if you merely add new methods and refrain from overriding existing methods. While this sort of extension is much safer, it is not without risk. If the superclass acquires a new method in a subsequent release and you have the bad luck to have given the subclass a method with the same signature and a different return type, your subclass will no longer compile.

If you've given the subclass a method with the same signature and return type as the new superclass method, then you're now overriding it, so you're subject to the problems described earlier. Furthermore, it is doubtful that your method will fulfill the contract of the new superclass method, because that contract had not yet been written when you wrote the subclass method.

## Avoid all the problems with composition

Instead of extending an existing class, give your new class a private field that references an instance of the existing class. This design is called **composition** because the existing class becomes a component of the new one. Each instance method in the new class invokes the corresponding method on the contained instance of the existing class and returns the results. This is known as **forwarding**, and the methods in the new class are known as **forwarding methods**.

The result class will be rock solid, with no dependencies on the implementation details of the existing class. Even adding new methods to the existing class will have no impact on the new class.

### Solving the problems in ```InstrumentedHashSet```

The implementation of the class is broken into two pieces, the class itself and a reusable _forwarding class_, which contains all of the forwarding methods and nothing else:

```java
// Wrapper class - use composition in place of inheritance
public class InstrumentedSet<E> extends ForwardingSet<E> {
    private int addCount;

    public InstrumentedSet(Set<E> s) {
        super(s);
    }

    @Override
    public boolean add(E e) {
        addCount++;
        return super.add(e);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        addCount += c.size();
        return super.addAll(c);
    }

    public int getAddCount() {
        return addCount;
    }
}

// Reusable forwarding class
public class ForwardingSet<E> implements Set<E> {
    private final Set<E> s;

    public ForwardingSet(Set<E> s) {
        this.s = s;
    }

    @Override
    public void clear() {
        s.clear();
    }

    @Override
    public boolean contains(Object o) {
        return s.contains(o);
    }

    @Override
    public boolean isEmpty() {
        return s.isEmpty();
    }

    @Override
    public int size() {
        return s.size();
    }

    @Override
    public Iterator<E> iterator() {
        return s.iterator();
    }

    @Override
    public boolean add(E e) {
        return s.add(e);
    }

    @Override
    public boolean remove(Object o) {
        return s.remove(o);
    }

    @Override
    public boolean containsAll(Collection<?> collection) {
        return s.containsAll(collection);
    }

    @Override
    public boolean addAll(Collection<? extends E> collection) {
        return s.addAll(collection);
    }

    @Override
    public boolean removeAll(Collection<?> collection) {
        return s.removeAll(collection);
    }

    @Override
    public boolean retainAll(Collection<?> collection) {
        return s.retainAll(collection);
    }

    @Override
    public Object[] toArray() {
        return s.toArray();
    }

    @Override
    public <T> T[] toArray(T[] ts) {
        return s.toArray(ts);
    }

    @Override
    public boolean equals(Object other) {
        return s.equals(other);
    }

    @Override
    public int hashCode() {
        return s.hashCode();
    }

    @Override
    public String toString() {
        return s.toString();
    }
}
```

The ```InstrumentedSet``` class implements the ```Set``` interface and has a single constructor whose argument is also of type ```Set```. In essence, the class transforms one ```Set``` into another, adding the instrumentation functionality.

Unlike the inheritance-based approach, which works only for a single concrete class and requires a separate constructor for each supported constructor in the superclass, the wrapper class can be used to instrument any ```Set``` implementation and will work in conjunction with any preexisting constructor:

```java
Set<Instant> times = new InstrumentedSet<>(new TreeSet<>(cmp));
Set<E> s = new InstrumentedSet<>(new HashSet<>(INIT_CAPACITY));
```

The ```InstrumentedSet``` class can even be used to temporarily instrument a set instance that has already been used without instrumentation:

```java
static void walk(Set<Dog> dogs) {
    InstrumentedSet<Dog> iDogs = new InstrumentedSet<>(dogs);
    ... // Within this method use iDogs instead of dogs
}
```

The ```InstrumentedSet``` class is known as a *wrapper* class because each ```InstrumentedSet``` instance contains ("wraps") another ```Set``` instance. This is also known as the **Decorator Pattern** because the ```InstrumentedSet``` class "decorates" a set by adding instrumentation.

Sometimes the combination of composition and forwarding is loosely referred to as _delegation_. Technically it's not delegation unless the wrapper object passes itself to the wrapped object.

#### Disadvantages of wrapper classes

- Wrapper classes are not suited for use in *callback frameworks*, wherein objects pass self-references to other objects for subsequent invocations. Because a wrapped object doesn't know of its wrapper, it passes a reference to itself (```this```) and callbacks elude the wrapper. This is known as the **SELF problem**.
- It's tedious to write forwarding methods, but you have to write the reusable forwarding class for each interface only once, and forwarding classes may be provided for you. For example, Guava provides forwarding classes for all of the collection interfaces.

## When Inheritance is appropriate

Inheritance is appropriate only in circumstances where the subclass really is a subtype of the superclass.

Before writing ```class B extends A```, ask yourself: **Is every B really an A?** If you cannot truthfully answer yer to this question, B should not extend A. If the answer is no, it is often the case that B should contain a private instance of A and expose a different API: A is not an essential part of B, merely a detail of its implementation.

There is one last set of questions you should ask yourself before deciding to use inheritance in place of composition. **Does the lass that you contemplate extending have any flaws in its API? If so, are you comfortable propagating those flaws into your class's API?** Inheritance propagates any flaws in the superclass's API, while composition lets you design a new API that hides these flaws.
