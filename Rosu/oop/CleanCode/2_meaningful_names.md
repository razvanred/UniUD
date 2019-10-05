# Meaningful Names

## Use Intention-Revealing Names

- Names should reveal intent;
- If a name requires a comment, then the name does not reveal its intent.

```java
int d; // elapsed time in days
```

We should choose a name that specifies what is beign measured (```elapsedTime```) and the unit of that measure (```InDays```):

```java
int elapsedTimeInDays;
int daysSinceCreation;
int daysSinceModification;
int fileAgeInDays;
```

---

What is the porpouse of this code?

```java
public List<int[]> getThem() {
    List<int[]> list1 = new ArrayList<>();
    for(int[] x: theList) {
        if(x[0] == 4){
            list1.add(x);
        }
    }
    return list1;
}
```

There are no complex expressions here:

- Reasonable spacing and indentation;
- only three variables and two constants mentioned (```list1```, ```x```, ```theList```, 0, 2);
- no fancy classes or polimorphic methods.

The problem here is not the simplicity of the code, but the __implicity__ of the code. The code implicitly requires that we already know the answers to questions such as:

1. What kind of things are in ```theList```?
2. What is the significance of the zeroth subscript of an item in ```theList```?
3. What is the significance of the value 4?
4. How would I use the list being returned?

What we found out about the project above:

- Mine sweeper game;
- The board is a list of cells called ```theList```. We rename it to ```board```.
- Each cell on the board is represented by a single array;
- The zeroth subscript is the location of a **status value**;
- The status value of 4 means _flagged_.

Just by knowing this concepts we can improve the code considerably:

```java
public List<int[]> getFlaggedCells() {
    List<int[]> flaggedCells = new ArrayList<>();
    for(int[] cell: board) {
        if(cell[STATUS_VALUE] == FLAGGED){
            flaggedCells.add(cell);
        }
    }
    return flaggedCells;
}
```

It still has exactly the same level of complexity. But the code has become much more **explicit**.

We can go further and write a simple class for cells instead of using an array of ```int```s. It can include and intention-revealing function (call it ```isFlagged```) to hide the magic numbers.

```java
class Cell {
    private static final int FLAGGED = 4;
    private static final int STATUS_VALUE = 0;

    private final int[] array;

    public Cell(final int[] array) {
        this.array = array;
    }

    public boolean isFlagged() {
        return array[STATUS_VALUE] == FLAGGED;
    }
}

public List<Cell> getFlaggedCells() {
    final List<Cell> flaggedCells = new ArrayList<>();
    for(final Cell cell: board){
        if(cell.isFlagged()){
            flaggedCells.add(cell);
        }
    }
    return flaggedCells;
}
```

## Avoid Disinformation

- Programmers must avoid leaving false clues that obscure the meaning of the code.
- Even if you are coding an hypotenuse and ```hp``` looks like a good abbreviation, it could be disinformative.
- Do not refer to a grouping of accounts as an ```accountList``` unless it's actuallu a ```List``` (```bunchOfAccounts```, ```accountGroup``` or just plain ```accounts``` would be better).
- Beware of using names which vary in small ways (spot the difference between ```XYZControllerForEfficientHandlingOfStrings``` and ```XYZControllerForEfficientStorageOfStrings```).
- Spelling similar concepts similarly is _information_. Using inconsistent spelling is _disinformation_.
- Besides the fact that are disinformative names, Do NOT use as variable names (especially in combination) the lower-case L or the upper-case O:

```java
int O, O1, l;
int a = 1;
if (O == l) {
    a = O1;
} else {
    l = 01;
}
```

## Make Meaningful Distingtions

Programmers create problem for themselves when they create code solely to satisfy the compiler; for example, because you can't use the same name to refer to two different things in the same scope, you might be tempted to change one name in an arbitrary way. It is not sufficient to add number series or noise numbers, even though the compiler is satisfied. **If name must be different, then they should also mean something different.**

```java
public static void copyChars(char a1[], char a2[]){
    for (int i = 0; i < a1.length; i++){
        a2[i] = a1[i];
    }
}
```

Number-series naming is the opposite of intentional naming. Such names are not disinformative, they are noninformative; they provide no clue to the author's intention. The function reads much better when ```source``` and ```destination``` are used for the argument names.

There is nothing wrong with using prefix conventions like ```a``` and ```the``` so long as they make a meaningful distinction. For example, you might use ```a``` for all local variables and ```the``` for all function arguments. The problem comes in when you decide to call a variable ```theZork``` because you already have another variable named ```zork```.

Noise words are redundant. The word ```variable``` should never appear in a variable name. The word ```table``` should never appear in a table name.

```java
getActiveAccount();
getActiveAccounts();
getActiveAccountInfo();
```

How are the programmers in this project supposed to know which of these functions to call?

In absence of specific conventions, ```moneyAccount``` is indistinguishable from ```money```, ```customerInfo``` is indistinguishable from ```customer``` ect.

> Distinguish names in such a way the reader knows what the difference offer.

## Use Pronounceable Names

Make your names pronounceable. If you can't pronounce it, you can't discuss it without sounding like an idiot. This matters because **programming is a social activity**.

Compare

```java
class DtaRcd102 {
    private Date genymdhms;
    private Date modymdhms;
    private final String pszqint = "102";
}
```

to

```java
class Customer {
    private Date generationTimestamp;
    private Date modificationTimestamp;
    private final String recordId = "102";
}
```

## Use Searchable Names

Single-letter names and numeric constants have a particular problem in that they are not easy to locate across a body of text.

Single-letter names can ONLY be used as local variables inside short methods.

> The length of a name should correspond to the size of its scope.

If a variable or constant might be seen or used in multiple places in a body of code, it is imperative to give it a search-frindly name.

Compare

```java
for(int j = 0; j<34; j++){
    s += (t[j] * 4) / 5;
}
```

to

```java
int realDaysPerIdealDay = 4;
const int WORK_DAYS_PER_WEEK = 5;
int sum = 0;

for(int j = 0; j < NUMBER_OF_TASKS; j++){
    int realTaskDays = taskEstimate[j] * realDaysPerIdealDay;
    int realTaskWeeks = realTaskDays / WORK_DAYS_PER_WEEK;
    sum += realTaskWeeks;
}
```

## Avoid Encodings

Encoding type or scope information into names simply adds an extra burden of deciphering.

### Hungarian Notation

HN was considered to be pretty much important back in the Windows C API. The compiler did not check types in those days, so the programmers needed a crutch to help them remember the types.

In modern languages we have much richer type systems, and the compilers remember and enforce types.

So nowadays HN and other forms of type encoding are simply impediments.

```java
PhoneNumber phoneString;
// the name hasn't changed when the type changed!
```

### Member Prefix

You also don't need to prefix member variables with ```m_``` anymore. Your classes and functions should be small enough that you don't need them.

Compare

```java
public class Part {
    private String m_dsc; // The textual description
    void setName(String name){
        m_dsc = name;
    }
}
```

to

```java
public class Part {
    private String name;
    void setName(String name){
        this.name = name;
    }
}
```

### Interfaces and Implementations

These are sometimes a special case for encodings.

Say you are building an Abstraction Factory for the creation of the shapes. What should you name them? ```IShapeFactory``` and ```ShapeFactory```?

I don't want my users knowing that I'm handing them an interface: I just want them to know that it's a ```ShapeFactory```.  Calling it ```ShapeFactoryImpl``` or even ```CShapeFactory``` is preferable to encoding the interface.

## Avoid Mental Mapping

This is a problem with single-letter variable names.

Certainly a loop counter may be named ```i``` or ```j``` (beware of ```l```) if its scope is very small and no other names can conflict with it. This is because those single-letter names for loop counters are traditional.

In most other contexts a single-letter name is a poor choice; it's just a place holder that the reader must mentally map the actual concept.

> One difference between a smart programmer and a professional programmer is that the professional programmer understands that _clarity is king_. Professionals use their powers for good and write code that others can understand.

## Class Names

- Noun or noun phrase names: ```Customer```, ```WikiPage```, ```AddressParser```;
- Avoid words like ```Manager```, ```Processor```, ```Data``` or ```Info``` in the name of the class;
- a class name should not be a verb.

## Methods Names

- Verb of verb phrase names like ```postPayment```, ```deletePage```, ```save```;
- Prefixes for accessors (```get```), mutators (```set```), and predicates (```is```) should be used according to the JavaBean standard.
- When constructors are overloaded, use static factory methods with names than describe the arguments, and enforce their use by making the corresponding constructors private.

```java
Complex fulcrumPoint = Complex.fromRealNumber(23.0);
```

is generally better than

```java
Complex fulcrumPoint = new Complex(23.0);
```

## Don't be cute

If names are too clever, they will be memorable only to people who share the author's sense of humor, and only as long as these people remember the joke.

Say what you mean. Mean what you say.

## Pick One Word per Concept

Pick one word for one abstract concept and stick with it. For instance, it's confusing to have ```fetch```, ```get``` and ```retreive``` as equivalent methods of different classes.

The function names have to stand alone, and they have to be consistent in order for you to pick the correct method withot any additional exploration from the list of methods you can call (on a given object) given by the IDE.

> A consistent lexicon is a great boon to the programmers who must use your code.

## Don't Pun

Avoid using the same word for two purposes. Using the same term for two different ideas is essentialy a pun.

If you follow the _one word per concept_ rule, you could end up with many classes that have, for example, an ```add``` method. As long as the parameter lists and return values of the various ```add``` methods are semantically equivalent, all is well.

## Use Solution Domain Names

The people who read your code will be programmers. So go ahead and use computer science terms, algorithm names, pattern names, math terms, and so forth. It is not wise to draw every name from the problem domain because we don't want our coworkers to have to run back and forth to the customer asking what every name means when they already know the concept by a different name.

## Use Problem Domain Names

The code that has more to do with problem domain concepts should have names drawn from the problem domain. The programer who maintains your code can ask a domain expert what it means.

> Separating solution and problem domain concepts is part of the job of a good programmer and designer.

## Add Meaningful Context

You need to place names in context for your reader by enclosing them in well-named classes, functions, or namespaces.

You can add context by using prefixes as a last restort: ```addrFirstName```, ```addrLastName```, ```addrState``` and so on. At least readers will understand that these variables are part of a larger structure. A better solution is to create a class named ```Address```. Then, even the compiler knows that the variable belong to a bigger concept.

Variables with unclear context:

```java
private void printGuessStatistics(char candidate, int count) {
    String number;
    String verb;
    String pluralModifier;

    if(count == 0) {
        number = "no";
        vert = "are";
        pluralModifier = "s";
    } else if (count == 1) {
        number = Integer.toString(count);
        verb = "is";
        pluralModifier = "";
    } else {
        number = Integer.toString(count);
        verb = "are";
        pluralModifier = "s";
    }

    String guessMessage = String.format("There %s %s %s%s", verb, number, candidate, pluralModifier);
    print(guessMessage);
}
```

The context here must be inferred. The function is a bit too long and the variables are used throughout.

Variables have context:

```java
public final class GuessStatisticsMessage {
    private final String number;
    private final String verb;
    private final String pluralModifier;

    public String make(final char candidate, int count) {
        createPluralDependentMessageParts(count);
        return String.format("There %s %s %s%s", verb, number, candidate, pluralModifier);
    }

    private void createPluralDependentMessageParts(final int count) {
        if(count == 0){
            thereAreNoLetters();
        } else if (count == 1){
            thereIsOneLetter();
        } else {
            thereAreManyLetters(count);
        }
    }

    private void thereAreNoLetters() {
        number = "no";
        verb = "are";
        pluralModifier = "s";
    }

    private void thereIsOneLetter() {
        number = "1";
        verb = "is";
        pluralModifier = "";
    }

    private void thereAreManyLetters(final int count) {
        number = Integer.toString(count);
        verb = "are";
        pluralModifier = "s";
    }
}
```

### Don't Add Gratuitous Context

In an imaginary application called "Gas Station Deluxe", it is a bad idea to prefix every class with ```GSD```. You type ```G``` and press the completion key and are shown a list of every class in the system.

Shorter names are generally better than longer ones, so long as they are clear. Add no more context to a name than is necessary.
