# Object Oriented Programming

## Objects's methods

1. `toString()`
2. `clone()`
3. `equals()`
4. `hashCode()`
5. `finalize()`
6. `getClass()`
7. `wait()`
8. `notify()`
9. `notifyAll()`

## common unchecked runtime exceptions

1. `RuntimeException`
2. `IllegalStateException`
3. `IllegalArgumentException`
4. `NullPointerException`
5. `UnsupportedOperationException`

## functional interfaces

1. `(T) UnaryOperator`
2. `(T) BinaryOperator <T>`
3. `(Boolean) Predicate <T>`
4. `(R) Function <T,R>`
5. `(T) Supplier <T>`
6. `(void) Consumer <T>`

## SOLID

* **S**ingle responsibility principle\
  a class should have only one reason to change
* **O**pen/close principle\
  software entities (classes, modules, functions, etc.) should be open for extension, but closed for modification
* **L**iskow principle\
  objects in a program should be replaceable with instances of their subtypes without altering the correctness of that program
* **I**nterface segregation\
  many client-specific interfaces are better than one general purpose interface
* **D**ependency inversion\
  high-level modules should not depend on low-level modules. Both should depend on abstractions\
  abstractions should not depend on details. Details should depend on abstractions
