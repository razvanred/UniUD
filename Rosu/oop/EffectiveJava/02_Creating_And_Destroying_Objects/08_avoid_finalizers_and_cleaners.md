# Avoid finalizers and cleaners

**Finalizers are unpredictable, often dangerous, and generally unnecessary.** As of Java 9, finalizers have been deprecated, but they are still being used by the Java libraries. The Java 9 replacement for finalizers is _cleaners_. **Cleaners are less dangerous than finalizers, but still unpredictable, slow, and generally unnecessary.**

In C++, destructors are the normal way to reclaim the resources associated with an object, a necessary counterpart to constructors. In Java, the garbage collector reclaims the storage associated with an object when it becomes unreacheable, requiring no special effort on the part of the programmer. C++ destructors are also used to reclaim other nonmemory resources. In Java, a [```try```-with-resources or ```try-finally``` block](09_prefer_try-with-resources_to_try-finally.md) is used for this purpouse.

One shortcoming of finalizers and cleaners is that [there is no guarrantee they'll be executed promptly](https://docs.oracle.com/javase/specs/jls/se7/html/jls-12.html#jls-12.6). It can take arbitrarily long between the time that an object becomes unreachable and the time its finalizer or cleaner to close files because open file descriptors are a limited resources.

The promptness with which finalizers and cleaners are executed is primarily a function of the garbage collector algorithm, which varies widely across implementations. The behavior of a program that depends on the promptness of finalizer or cleaner execution may likewise vary.

Note: providing a finalizer for a class can arbitrary delay reclamation of its instances.

The language specification makes no guarantees as to which thread will execute finalizers. Cleaners are a bit better than finalizers in this regard because class authors have control over their own cleaner threads, but cleaners still run in background, under the control of garbage collector, so there can be no guarantee of prompt cleaning. As a consequence, **you should never depend on a finalizer or cleaner to update persistent state**.

Don't be seduced by the methods ```System.gc``` and ```Runtime.runFinalization```. They may increase the odds of finalizers or cleaners getting executed, but they don't guarantee it.

Another problem with finalizers is that [an uncaught exception thrown during finalization is ignored, and finalization of that object terminates](https://docs.oracle.com/javase/specs/jls/se7/html/jls-12.html#jls-12.6).

**There is a severe performance penality for using finalizers and cleaners:** finalizers inhibit efficient garbage collection. Cleaners are comparable in speed to finalizers if you use them to clean all instances of the class, but cleaners are much faster if you use them only as a safety net.

## Finalizers Attacks

**Finalizers have a serious security problem: they open your class up to *finalizers attacks*.**

If an exception thrown from a constructor or its serialization equivalents, the finalizer of a malicious subclass can run on the partially constructed object that should have "died on the vine". This finalizer can record a reference to the object in a static field, preventing it from being garbage collected. Once the malformed object has been recorded, it is a simple matter to invoke arbitrary methods on this object that should have never been allowed to exist in the first place. **Throwing an exception from a constructor should be sufficient to prevent an object from coming into existence; in the presence of finalizers, it is not.**

How to prevent:

* Final classes are immune to finalizer attacks because no one can write a malicious subclass of a final class.
* <mark style="background-color: lightgreen">To protect nonfinal classes from finalizers attacks, write a final ```finalize``` method that does nothing.</mark>

## The ```AutoCloseable``` interface

Instead of writing a finalizer or cleaner for a class whose objects encapsulate resources that require termination, such files or threads, have your class implement ```AutoCloseable```, and require its clients to invoke the ```close``` method on each instance when it is no longer needed, typically using [```try```-with-resources](09_prefer_try-with-resources_to_try-finally.md) to ensure termination even in the face of exceptions.

Note: The ```close``` method must record in a field that the object is no longer valid, and other methods must check this field and throw ```IllegalStateException``` if they are called after the object has been closed.

## Reasons behind the existance of finalizers and Cleaners

1. They act as a safety net in case the owner of a resource neglects to call its ```close``` method. While there's no guarantee that the cleaner or finalizer will run promptly (or at all), it is better to free the resource late than never if the client fails to do so.
2. Objects with _native peers_ (native non-Java object to which a normal object delegates via native methods): because the native peer isn't a normal object, the garbage collector doesn't know about it and can't reclaim it when its Java peer is reclaimed. If the native peer holds resources that must be reclaimed promptly, the class should have a ```close``` method, as described earlier.

## Cleaner as a safety net

Unlike finalizers, Cleaners do not pollute a class's public API:

```java
public class Room implements AutoCloseable {
    private final static Cleaner cleaner = Cleaner.create();

    // Resource that requires cleaning. Must not refer to Room! (avoid non-static class definition)
    private static final class State implements Runnable {
        private int numJunkPiles;

        private State(final int numJunkPiles) {
            this.numJunkPiles = numJunkPiles;
        }

        // Invoked by close method or cleaner
        @Override
        public void run() {
            System.out.println("Cleaning room");
            numJunkPiles = 0;
        }
    }

    private final State state;
    private final Cleaner.Cleanable cleanable;

    public Room(final int numJunkPiles) {
        this.numJunkPiles = numJunkPiles;
        cleanable = cleaner.register(this, state); // (Object, Runnable)
    }

    @Override
    public void close() {
        cleanable.clean();
    }
}
```

It is critical that a ```State``` instance does not refer to its ```Room``` instance. If it did, it would create a circularity that would prevent the ```Room``` instance from becoming eligible for garbage collection (and from being automatically cleaned). Therefore: ```State``` must be a static nested class because [nonstatic nested classes contain references to their enclosing instances](../04_Classes_and_Interfaces/24_favor_static_member_classes_over_nonstatic.md). It is similarly inadvisable to use a lambda because they can easily capture references to enclosing objects.

Usage:

```java
public class Adult {
    public static void main(String[] args) {
        try (final var room = new Room(7)){
            System.out.println("Goodbye");
        }
    }
}
// Output:
// Goodbye
// Cleaning room
```

Ill-behaved program that never cleans its room:

```java
public class Teenager {
    public static void main(String[] args) {
        new Room(99);
        System.out.println("Peace out");
    }
}
// Output:
// Peace out
```
