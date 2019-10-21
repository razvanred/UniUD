# Eliminate obsole object references

There are 3 common sources of memory leaks.

## 1. Classes that manage their own memory

Spot the **memory leak**:

```java
public class Stack {
    public Object[] elements;
    public int size = 0;
    private static final int DEFAULT_INITIAL_CAPACITY = 16;

    public Stack() {
        elements = new Object[DEFAULT_INITIAL_CAPACITY];
    }

    public void push(Object e) {
        ensureCapacity();
        elements[size++] = e;
    }

    public Object pop() {
        if(size == 0) {
            throw new EmptyStackException();
        }
        return elements[--size];
    }

    /**
     * Ensure space for at least one more element, roughly
     * doubling the capacity each time the array needs to grow.
     */
    private void ensureCapacity() {
        if(elements.length == size) {
            elements = Arrays.copyOf(elements, 2 * size + 1)
        }
    }
}
```

If a stack grows and then shrinks, the objects that were popped off the stack will not be garbage collected, even if the program using the stack has no more references to them. This is because the stack is maintaining _obsolete references_ to these objects.

Memore leaks in garbage-collected languages (more properly known as _unintentional object retentions_) are insidious. If an object reference is unintentionally retained, not only is that object excluded from garbage collection, but so too are any objects referenced by that object, and so on.

Fixing the memory leak:

```java
public Object pop() {
    if(size == 0) {
        throw new EmptyStackException();
    }
    final Object result = elements[--size];
    elements[size] = null; // Eliminate obsolete reference
    return result;
}
```

Note: **Nulling out object references should be the exception rather than the norm.** The best way to eliminate an obsole reference is to let the variable that contained the reference fall out of scope. This occurs naturally if you define each variable in the [narrowest possible scope](../09_General_Programming/57_minimize_the_scope_of_local_variables.md).

_When should you null out a reference?_ The ```Stack``` class manages its own memory. The _storage pool_ consists of the elements of the ```elements``` array (the object reference cells, not the objects themselves). The elements in the active portion of the array (as defined earlier) are _allocated_, and those in the remainder of the array are _free_. The programmer communicates with the garbage collector by manually nulling out array elements as soon as they become part of the inactive portion.

In general, **whenever a class manages its own memory, the programmer should be alert for memory leaks.** Whenever an element is freed, any object reference contained in the element should be nulled out.

## 2. Caches

If you have to implement a cache for which an entry is relevant exactly so long as there are references to its key outside of the cache, represent the cache as a ```WeakHashMap```; entries will be removed automatically after they become obsolete.

Otherwise, the cache should occasioanlly be cleanesed of entries that have fallen into disuse. This can be done by a background thread (perhaps a ```ScheduledThreadPoolExecutor```) or as a side effect of adding new entries to the cache. The latter approach can be facilitated with the class ```LinkedHashMap``` with its ```removeEldestEntry``` method.

## 3. Listeners and Other Callbacks

If you implement an API where clients register callbacks but don't deregister them explicitly, they will accumulate unless you take some action. One way to ensure that callbacks are garbage collected promptly is to store only _weak references_ to them, for instance, by storing them only as keys in ```WeakHashMap```.
