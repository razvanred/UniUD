# Dependency Injection with Dagger 2

## ```@Component``` interfaces/abstract classes

It tells Dagger to implement an ```interface``` or an ```abstract class``` that creates and returns one or more application objects.

Dagger will generate a class that implements the component type. The generated type will be named ```DaggerYourType``` (or ```DaggerYourType_NestedType``` for nested types).

```java
@Component
interface CommandRouterFactory {
    CommandRouter router();
    
    static CommandRouterFactory create() {
        return DaggerCommandRouterFactory.create();
    }
}
```

## ```@Inject``` constructors

* On a constructor it tells Dagger how to instantiate that class;

```java
public class HelloCommand {
    @Inject
    public HelloCommand() { }
}
```

* Parameters to an ```@Inject``` constructor are the dependencies of the class. Dagger will provide a class's dependencies to instantiate the class itself.

```java
public class HelloCommand {
    private final Outputter outputter;
    
    @Inject
    public HelloCommand(final Outputter outputter) {
        this.outputter = outputter;
    }
}
```

Note that this is **recursive**: a class may have dependencies of its own.

## ```Module```(s) interfaces/abstract classes

* They act as collections of instructions for Dagger on how to construct dependencies.
* They are _modular_: you can mix and match modules in different applications and contexts.

```java
@Module
interface AmountsModule {
    @Provides
    static BigDecimal maximumWithdrawal() {
        return new BigDecimal(1_000);
    }
}
```

## ```@Binds``` methods

* Abstract methods that tell Dagger how to construct an instance;
* They associate one type that already Dagger knows how to construct (the method's parameter) with a type that Dagger doesn't yet know how to construct (the method's return type).

```java
@Module
interface CommandsModule {
    @Binds
    @IntoMap
    @StringKey("login")
    Command login(final LoginCommand command);

    @Binds
    @IntoMap
    @StringKey("logout")
    Command logout(final LogoutCommand command);
}
```

## ```@Provides``` methods

* Concrete methods in a module that tells Dagger that when something requests an instance of that type the method returns, it should call that method to get an instance.
* Like ```@Inject``` constructors, they can have parameters: those parameters are their dependencies;
* They can contain arbitrary code as long as they return an instance of the provided type.

## Multibindings methods

The generated collection contains elements from several different binding methods.

### ```@IntoMap``` methods

* They allow for the creation of a map with values of a specific type, with keys set using special annotations such as ```@StringKey``` or ```@IntKey```;
* Because keys are set via annotation, Dagger ensures that multiple values are not mapped to the same key.

### ```@IntoSet``` methods

They allow for the creation of a set of types to be collected together. It can be used together with ```@Binds``` and ```@Provides``` to provide a ```Set<ReturnType```.

## ```@Singleton``` class

* This annotation instructs Dagger to create only one instance of the type for each instance of the component.
* It can be used on the class declaration of a type that has an ```@Inject``` constructor, or on a ```@Binds``` or ```@Provides``` method.

```java
@Singleton
public class Database {
    @Inject
    public Database() { }
}
```

## ```@Subcomponent``` interface/abstract class

* It is like a ```@Component```-annotated one, a factory for an object. Like ```@Component``` it uses modules to give Dagger implementation instructions.
* Subcomponents always have a parent component/subcomponent, and any objects that are requestable in the parent are requestable in the child, but not vice versa.
* ```@Subcomponent.Factory``` is like ```@Component.Factory```, it creates instances of the subcomponent. An instance of it is requestable in the parent component.

```java
@Subcomponent(modules = {UserCommandsModule.class})
public interface UserCommandsRouter {
    CommandsRouter router();
    
    @Subcomponent.Factory
    interface Factory {
        UserCommandsRouter create(@BindsInstance final Account account);
    }

    @Module(subcomponents = {UserCommandsRouter.class})
    interface InstallationModule { }
}
```

## ```@BindsInstance``` parameters

They let you make arbitrary objects requestable by other binding methods in the component.

## ```@Qualifier``` annotations

* They are used to differentiate between instances of the same type that are unrelated.
  * Contrast this with ```@IntoSet``` and ```@IntoMap```, where the collected objects are used together.
* Qualifiers are often used with common data types such as primitive types and ```String```, which may be used in many places in a program for very different reasons. 

```java
@Qualifier
@Retention(RetentionPolicy.RUNTIME)
public @interface MaximumWithdrawal { }

@Qualifier
@Retention(RetentionPolicy.RUNTIME)
public @interface MinimumAmount { }

@Module
interface AmountsModule {
    @Provides
    static @MaximumWithdrawal BigDecimal maximumWithdrawal() {
        return new BigDecimal(1_000);
    }
    @Provides
    static @MinimumAmount BigDecimal minimumAmount() {
        return BigDecimal.ZERO;
    }
}
```

## ```@Scope``` annotation

* It tells Dagger to provide one shared instance for all the requests for that type within an instance of the (sub)component that shares the same annotation.
  * Note that ```@Singleton``` is just another scope annotation!
* The lifetime of a scoped instance is directly related to the lifetime of the component within that scope.


```java
@Scope
@Retention(RUNTIME)
public @interface PerSession { }

@PerSession
@Subcomponent(modules = {UserCommandsModule.class})
public interface UserCommandsRouter {
    CommandsRouter router();
}
```

## ```@BindsOptionalOf``` method

It tells Dagger that it can construct instances of ```Optional<ReturnType>```. The presence of the ```Optional``` is determined by whether Dagger knows how to create an instance of ```ReturnType```, and it can be present in a subcomponent but absent in its parent.

```java
@Module
public interface CommandsModule {

    @Binds
    @IntoMap
    @StringKey("hello")
    Command helloCommand(final HelloCommand command);

    @Binds
    @IntoMap
    @StringKey("login")
    Command loginCommand(final LoginCommand command);

    @BindsOptionalOf
    Account optionalAccount();

}
```
