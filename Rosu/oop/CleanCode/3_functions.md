# Functions

```HtmlUtil.java``` (FitNesse 20070619, Listing 3-1)

```java
public static String testableHtml(
    PageData pageData,
    boolean includeSuiteSetup
) throws Exception {
    WikiPage wikiPage = pageData.getWikiPage();
    StringBuffer buffer = new StringBuffer();
    if(pageData.hasAttribute("Test")) {
        if(includeSuiteSetup) {
            WikiPage suiteSetup =
                PageCrawlerImpl.getInheritedPage(
                    SuiteResponder.SUITE_SETUP_NAME, wikiPage
                );
            if(suiteSetup != null) {
                WikiPagePath pagePath = suiteSetup.getPageCrawer().getFullPath(suiteSetup);
                String pagePathName = PathParser.render(pagePath);
                buffer.append("!include -setup .")
                    .append(pagePathName)
                    .append("\n");
            }
        }
        WikiPage setup = PageCrawlerImpl.getInheritedPage("SetUp", wikiPage);
        if(setup != null) {
            WikiPagePath setupPath = setup.getPageCrawler().getFullPath(setup);
            String setupPathName = PathParser.render(setupPath);
            buffer.append("!include -setup .")
                .append(setupPathName)
                .append("\n");
        }
    }
    buffer.append(pageData.getContent());
    if(pageData.hasAttribute("Test")) {
        WikiPage teardown = PageCrawlerImpl.getInheritedPage("TearDown", wikiPage);
        if(teardown != null) {
            WikiPagePath tearDownPath = wikiPage.getPageCrawler().getFullPath(teardown);
            String tearDownPathName = PathParser.render(tearDownPath);
            buffer.append("\n")
                .append("!include -teardown .")
                .append(tearDownPathName)
                .append("\n");
        }
        if(includeSuiteSetup) {
            WikiPage suiteTeardown = PageCrawlerImpl.getInheritedPage(
                SuiteResponder.SUITE_TEARDOWN_NAME,
                wikiPage
            );
            if(suiteTeardown != null) {
                WikiPagePath suiteTeardown.getPageCrawler().getFullPath(suiteTeardown);
                String pagePathName = PathParser.render(pagePath);
                buffer.append("!include -teardown .")
                    .append(pagePathName)
                    .append("\n");
            }
        }
    }
    pathData.setContent(buffer.toString());
    return pageData.getHtml();
}
```

```HtmlUtil.java``` (refactored, Listing 3-2)

```java
public static String renderPageWithSetupsAndTeardowns(
    PageData pageData, boolean isSuite
) throws Exception {
    boolean isTestPage = pageData.hasAttribute("Test");
    if(isTestPage) {
        WikiPage testPage = pageData.getWikiPage();
        StringBuffer newPageContent = new StringBuffer();
        includeSetupPages(testPage, newPageContent, isSuite);
        newPageContent.append(pageData.getContent());
        includeTeardownPages(testPage, newPageContent, isSuite);
        pageData.setContent(newPageContent.toString());
    }

    return pageData.toHtml();
}
```

## Small

The first rule of functions is that they should be small. The second rule of functions is that _they should be smaller than that_.

```HtmlUtil.java``` (re-refactored, Listing 3-3)

```java
public static String renderPageWithSetupsAndTeardowns(
    PageData pageData, boolean isSuite
) throws Exception {
    if(isTestPage(pageData)) {
        includeSetupAndTeardownPages(pageData, isSuite);
    }
    return pageData.getHtml();
}
```

### Blocks and Indenting

The blocks within ```if``` statements, ```else``` statements, ```while``` statements and so on should be one line long. Probably that line should be a function call. Not only does this keep the enclosing function small, but it also adds documentary value because the function called within the block can have a nice descriptive name.

This also implies that functions should not be large enough to hold nested structures. Therefore, the indent level of a function should not be greater than one or two. This makes the functions easier to read and understand.

## Do One Thing

**Functions should do one thing. They should do it well. They should do it only.**

The problem with this statement is that it is hard to know what "one thing" is. Does Listing 3-3 do one thing? It's easy to make the case that it's doing three things:

1. Determining whether the page is a test page.
2. If so, including setups and teardowns.
3. Rendering the page in HTML.

The three steps of the function are one level of abstraction below the stated name of the function. We can describe the function by describing it as a brief _TO_ paragraph:

> TO RenderPageWithSetupsAndTeardowns, we check to see whether the page is a test page and if so, we include the setups and teardown. In either case we render the page in HTML.

If a function does only those steps that are one level below the stated name of the function, than the function is doing one thing. After all, the reason we write functions is to decompose a larger concept (in other words, the name of the function) into a set of steps at the next level of abstraction.

We could extract the ```if``` statement into a function named ```includeSetupsAndTeardownsIfTestPage```, but that simply restates the code without changing the level of abstraction.

So, another way to know that a function is doing more than "one thing" is if you can extract another function from it with a name that is not merely a restatement of its implementation.

### Sections within Functions

```java
import java.util.*;

public class GeneratePrimes {
    public static int[] generatePrimes(int maxValue) {
        if(maxValue >= 2) {
            // declarations
            int s = maxValue + 1;
            boolean[] f = new boolean[s];
            int i;

            // initialize array to true
            for(i = 0; i < s; i++)
                f[i] = true;

            // get rid of known non-primes
            f[0] = f[1] = false;

            //sieve
            int j;
            for(i = 2; i < Math.sqrt(s) + 1; i++) {
                if(f[i]) { // if i is uncrossed, cross its multiples
                    for(j = 2 * i; j < s; j += i)
                        f[j] = false; // multiple is not prime
                }
            }

            // how many primes are there?
            int count = 0;
            for(i = 0; i < s; i++) {
                if(f[i])
                    count++; // bump count.
            }

            int[] primes = new int[count];

            // move the primes into the result
            for(i = 0, j = 0; i < s; i++) {
                if(f[i])
                    primes[j++] = i;
            }

            return primes;
        } else
            return new int[0];
    }
}
```

Notice that this function is divided into sections such as _declarations_, _initializations_ and _sieve_. This is an obvious symptom of doing more than one thing. Functions that do one thing cannot be reasonably divided into sections.

## One Level of Abstraction per Function

In order to make sure our functions are doing "one thing", we need to make sure that the statements within our function are all at the same level of abstraction.

Mixing levels of abstraction within a function is always confusing. Readers may not be able to tell whether a particular expression is an essential concept or a detail.

### Reading Code from Top to Bottom: _The Stepdown Rule_

We want the code to read like a top-down narrative. We want every function to be able to read the program as though it were a set of _TO_ paragraphs, each of which is describing the current level of abstraction and referencing subsequent _TO_ paragraphs at the next level down.

> TO include the setups and teardowns, we include setups, then we include the test page content, and then we include the teardowns.
>
> TO include the setups, we include the suite setup if this is a suite, then we include the regular setup.
>
> TO include the suite setup, we search the parent hierarchy for the "SuiteSetUp" page and add an include statement with the path of that page.
>
> TO search the parent...

Making the code read like a top-down set of _TO_ paragraphs is an effective technique for keeping the abstraction level consistent.

### Switch Statements

It's hard to make a small ```switch``` statement. It's also hard to make a ```switch``` statement that does one thing. By their nature, ```switch``` statements always do _N_ things. Unfortunately we can't always avoid ```switch``` statements, but we can make sure that each ```switch``` statement is buired in a low-level class and is never repeated. We do this, of course, with polymorphism.

```java
public Money calculatePay(final Employee e) throws InvalidEmployeeType {
    switch(e.type) {
        case COMMISSIONED:
            return calculateCommissionedPay(e);
        case HOURLY:
            return calculateHourlyPay(e);
        case SALARIED:
            return calculateSalariedPay(e);
        default:
            throw new InvalidEmployeeType(e.type);
    }
}
```

There are several problem with this function:

1. It's large, and when new employee types are added, it will grow.
2. It does more than one thing.
3. It violates the **Single Responsability Principle** because there is more than one reason for it to change.
4. It violates the **Open-Closed Principle** because it must change whenever new types are added.
5. There are unlimited number of other functions that will have the same structure, like:

     ```java
     isPayDay(Employee e, Date date)
     ```

     or

     ```java
     deliverPay(Employee e, Money pay)
     ```

The solution to this problem is to bury the ```switch``` statement in the basement of an **Abstract Factory**, and never let anyone see it. The factory will use the ```switch``` statement to create appropriate instances of the derivatives of ```Employee```, and the various functions, such as ```calculatePay```, ```isPayDay``` and ```deliverPay```, will be dispatched polymorphically through the ```Employee``` interface.

Uncle Bob's general rule for ```switch``` statements is that they can be tolerated if they appear only one, are used to create polymorphic objects, and are hidden behind an inheritance relationship so that the rest of the system can't see them.

```java
public abstract class Employee {
    public abstract boolean isPayDay();
    public abstract Money calculatePay();
    public abstract void deliverPay();
}

public interface EmployeeFactory {
    Employee makeEmployee(EmployeeRecord r) throws InvalidEmployeeType;
}

public class EmployeeFactoryImpl implements EmployeeFactory {
    public Employee makeEmployee(EmployeeRecord r) throws InvalidEmployeeType {
        switch(r.type) {
            case COMMISSIONED:
                return new CommissionedEmployee(r);
            case HOURLY:
                return new HourlyEmployee(r);
            case SALARIED:
                return new SalariedEmployee(r);
            default:
                throw new InvalidEmployeeType(r.type);
        }
    }
}
```

## Use Descriptive Names

Ward's principle:

> You know you are working on clean code when each routine turns out to be pretty much what you expected.

* The smaller and more focused a function is, the easier it is to choose a descriptive name.
* Don't be afraid to make a name long. A long descriptive name is better than a short enigmatic name. A long descriptive name is better than a long descriptive comment.
* Be consistent in your names. Use the same phrases, nouns, and verbs in the function names you choose for your modules.

## Function Arguments

The ideal number of arguments for a function is zero (niladic). Next comes one (monadic), followed closely by two (dyadic). Three arguments (triadic) should be avoided where are possible. More than three (polyadic) requires very special justification - and then shouldn't be used anyway.

When you are reading the story told by the module, ```includePageSetup()``` is easier to understand than ```includeSetupPageInto(newPageContent)```. The argument is at a different level of abstraction than the function name and forces you to know a detail (in other words, ```StringBuffer```) that isn't particularly important at that point.

**Arguments are even harder from a testing point of view.** with more than two arguments, testing every combination of appropriate values can be daunting.

**Output arguments are harder to understand than input arguments.** When we read a function, we are used to the idea of information going _in_ to the function through arguments and _out_ through the return value. We don't usually expect information to be going out through the arguments.

One input argument is the next best thing to no arguments. ```SetupTeardownIncluder.render(pageData)``` is pretty easy to understand. Clearly we are going to _render_ the data in the ```pageData``` object.

### Common Monadic Forms

There are two very common reasons to pass a single argument into a function:

* You may be asking a question about that argument, as in ```boolean fileExists("MyFile")```.
* You may be operating on that argument, transforming it into something else and _returning it_, like ```InputStream fileOpen("MyFile")```.

A somewhat less common, but still very useful form for a single argument function, is an _event_. In this form there is an input argument but no output argument. The overall program is meant to interpret the function call as an event and use the argument to alter the state of the system, for example, ```void passwordAttemptFailedNtimes(int attempts)```. Use this form with care. It should be ery clear to the reader that this is an event. Choose names and contexts carefully.

Try to avoid any monadic functions that don't follow these forms, for example, ```void includeSetupPageInto(StringBuffer pageText)```. Using an output argument instead of a return value for a transformation is confusing. If a function is going to transform its input argument, the transformation should appear as the return value. Indeed, ```StringBuffer transform(StringBuffer in)``` is better than ```void transform(StringBuffer out)```, even if the implementation in the first case simply returns the input argument.

#### Flag Arguments

Flag arguments are ugly. Passing a boolean into a function is a truly terrible practice. It immediately complicates the signature of the method, loudly proclaming that this function does more than one thing. It does one thing if the flag is true and another if the flag is false.

### Dyadic Functions

```writeField(name)``` is easier to understand than ```writeField(outputStream, name)```. Through the meaning of both is clear, the first glides past the eye, easily depositing its meaning. The second requires a short pause until we learn to ignore the first parameter. And _that_, of course, eventually results in problems because **we should never ignore any part of the code. The parts we ignore are where the bugs will hide.**

There are times, of course, where two arguments are appropriate. For example, ```Point p = new Point(0, 0)``` is perfectly reasonable. The two arguments in this case _are ordered components of a single value_. Whereas ```outputStream``` and ```name``` have neither a natural cohesion, nor a natural ordering.

Even obvious dyadic functions like ```assertEquals(expected, actual)``` are problematic: the two arguments have no natural ordering. The ```expected```, ```actual``` ordering is a convention that requires practice to learn.

Dyads aren't evil, but you should take advantage of what mechanisms may be available to you to convert them into monads.

For example, you might make the ```writeField``` method a member of ```outputStream``` so that you can say ```outputStream.writeField(name)```. Or you might make ```outputStream``` a member variable of the current class so that you don't have to pass it. Or you might extract a new class like ```FieldWriter``` that takes the ```outputStream``` in its constructor and has a ```write``` method.

### Triadic Functions

Functions that take three arguments are significally harder to understand than dyads. The issues of ordering, pausing, and ignoring are more than doubled.

Consider the overload of ```assertEquals``` that takes three arguments: ```assertEquals(message, expected, actual)```.

### Argument Objects

When a function seems to need more than two or three arguments, it is likely that some of those arguments ought to be wrapped into a class of their own.

Consider the difference between the two following declarations:

```java
Circle makeCircle(double x, double y, double radius);
Circle makeCircle(Point center, double radius);
```

When group of variables are passed together, the way ```x``` and ```y``` are in the example above, they are likely part of a concept that deserves a name of its own.

### Argument Lists

```java
String.format("%s worked %.2f hours.", name, hours);
```

If the variable arguments are all treated identically, as they are in the example above, then they are equivalent to a single argument of type ```List```. By that reasoning, ```String.format``` is actually dyadic. Indeed, the declaration of ```String.format``` as shown below is clearly dyadic.

```java
public String format(String format, Object... args);
```

So all the same rules apply. Functions that take variable arguments can be monads, dyads, or even triads.

```java
void monad(String... args);
void dyad(String format, Object... args);
void triad(String name, int count, Object... args);
```

### Verbs and Keywords

In the case of a monad, the function and argument should form a very nice verb/noun pair. For example, ```write(name)``` is very evocative. Whatever this "name" thing is, it is being written. An even better name might be ```writeField(name)```, which tells us that the name thing is a field.

This last is an example of the _keyword_ form of a function name. Using this form we encode the names of the arguments into the function name. For example, ```assertEquals``` might be better written as ```assertExpectedEqualsActual(expected, actual)```. This strongly mitigates the problem of having to remember the ordering of the arguments.

## Have No Side Effects

Side effects are lies. Your function promises to do one thing, but it also does other _hidden_ things. Sometimes it will make unexpected changes to the variales of its own class. Sometimes it will make them to the parameters passed into the function or to system globals.

Consider the function written in ```UserValidator.java```. This function uses a standard algorithm to match a ```userName``` to a ```password```. It returns ```true``` if they match and ```false``` if anything goes wrong. But it has also a side effect.

```java
public class UserValidator {
    private Cryptographer cryptographer;

    public boolean checkPassword(String userName, String password) {
        User user = UserGateway.findByName(userName);
        if(user != null) {
            String codedPhrase = user.getPhraseEncodedByPassword();
            String phrase = cryptographer.decrypt(codedPhrase, password);
            if("Valid Password".equals(phrase)) {
                Session.initialize();
                return true;
            }
        }
        return false;
    }
}
```

The side effect is to call ```Session.initialize()```. The name does not imply that it initializes the session.

This side effect creates a temporal coupling. That is, ```checkPassword``` can only be called at certain times (in other words, when it is safe to initialize the session). If it is called out of order, session data may be inadvertenly lost. If you must have a temporal coupling you should make it clear in the name of the function. In this case we might rename the function ```checkPasswordAndInitializeSession```, though that certainly violates "Do one thing".

### Output Arguments

Arguments are most naturally interpreted as inputs to a function. For example:

```java
appendFooter(s);
```

Does this function append ```s``` as the footer to something? Or does it append some footer to ```s```? Is ```s``` an input or an output? It doesn't take long to look at the function signature and see:

```java
public void appendFooter(StringBuffer report);
```

This clarifies the isses, but only at the expense of checking the declaration of the function.

**Anything that forces you to check the function signature is equivalent to a double-take. It's a cognitive break and should be avoided.**

In general , output arguments should be avoided. If your function must change the satte of something, have it change the state of its owning object.

## Command Query Separation

Functions should either do something or answer something, but not both. Either your function should change the state of an object, or it should return some information about that object. Doing both often leads to confusion. Consider, for example, the following function:

```java
public boolean set(String attribute, String value);
```

This function sets the value of a named attribute and returns ```true``` if it is successful and ```false``` if no such attribute exists. This leads to odds statements like this:

```java
if(set("username", "red"))
```

What does it mean? Is it asking whether the ```"username"``` attribute was previously set to ```"red"```? Or it is asking whether the ```"username"``` attribute was successfully set to ```"red"```? It's hard to infer the meaning from the call because it is not clear whether the word ```set``` is a verb or an adjective.

The author intended ```set``` to be a verb, but in the context of the ```if``` statement it feels like an adjective.

We could try to resolve this by renaming the ```set``` function to ```setAndCheckIfExists```, but that doesn't much help the readability of the ```if``` statement. The real solution is to separate the command from the query so that the ambiguity cannot occur.

```java
if(attributeExists("username")) {
    setAttribute("username", "red");
}
```

## Prefer Exceptions to Returning Error Codes

Returning error codes from command functions is a subtle violation of command query separtion. It promotes commands being used as expressions in the predicates of the ```if``` statements.

```java
if(deletePage(page) == E_OK)
```

This does not suffer from verb/adjective confusion but does lead to deeply nested structures. When you return an error code, you create the problem that the caller must deal with the error immediately.

```java
if(deletePage(page) == E_OK) {
    if(registry.deleteReference(page.name) == E_OK) {
        if(configKeys.deleteKey(page.name.makeKey()) == E_OK) {
            logger.log("page deleted");
        } else {
            logger.log("configKey not deleted");
        }
    } else {
        logger.log("deleteReference from registry failed");
    }
} else {
    logger.log("delete failed");
    return E_ERROR;
}
```

On the other hand, if you use exceptions instead of returned error codes, then the error processing code can be separated from the happy path code and can be simplified:

```java
try {
    deletePage(page);
    registry.deleteReference(page.name);
    configKeys.deleteKey(page.name.makeKey());
} catch (Exception exc) {
    logger.log(exc.getMessage());
}
```

### Extract Try/Catch Blocks

```try/catch``` blocks are ugly in their own right. They confuse the structure of the code and mix error processing with normal processing. So it is better to extract the bodies of the ```try``` and ```catch``` blocks out into functions of their own.

```java
public void delete(Page page) {
    try{
        deletePageAndAllReferences(page);
    } catch (Exception exc) {
        logError(exc);
    }
}

private void deletePageAndAllReferences(Page page) throws Exception {
    deletePage(page);
    registry.deleteReference(page.name);
    configKeys.deleteKey(page.name.makeKey());
}

private void logError(Exception e) {
    logger.log(e.getMessage());
}
```

### Error Handling is One Thing

Functions should do one thing. Error handling is one thing. Thus, a function that handles errors should do nothing else. This implies (as in the example above) that if the keyword ```try``` exists in a function, it should be the very first word in the function and that there should be nothing after the ```catch/finally``` blocks.

### The ```Error.java``` Dependency Magnet

Returning error codes usually implies that there is some class or enum in which all the error codes are defined.

```java
public enum Error {
    OK,
    INVALID,
    NO_SUCH,
    LOCKED,
    OUT_OF_RESOURCES,
    WAITING_FOR_EVENT;
}
```

Classes like this are a _dependency magnet_; many other classes must import and use them. Thus, when the ```Error enum``` changes, all those other classes need to be recompiled and redeployed. This puts a negative pressure on the ```Error``` class.

When you use exceptions rather than error codes, then new exceptions are _derivatives_ of the exception class. They can be added without forcing any recompilation or redeployment.

## Don't Repeat Yourself

The duplication is a problem because it bloats the code and will require four-fold modification should the algorithm ever have to change. It is also a four-fold opportunity for an error of omission.

Duplication may be the root of all evil in software. Many principles and practices have been created for the porpouse of controlling or eliminating it.

## Structured Programming

If you keep your functions small, then the occasional multiple ```return```, ```break``` or ```continue``` statement does no harm and can sometimes even be more expressive than the single-entry, single-exit rule. On the other hand, ```goto``` only makes sense in large functions so it should be avoided.

## Conclusion

Every system is built from a domain-specific language designed by the programmers to describe that system. Functions are the verbs of that language, and classes are the nouns.

## ```SetupTeardownIncluder```

```java
public class SetupTeardownIncluder {
    private PageData pageData;
    private boolean isSuite;
    private WikiPage testPage;
    private StringBuffer newPageContent;
    private PageCrawler pageCrawler;

    public static String render(PageData pageData) throws Exception {
        return render(pageData, false);
    }

    public static String render(PageData pageData, boolean isSuite) throws Exception {
        return new SetupTeardownIncluder(pageData).render(isSuite);
    }

    private SetupTeardownIncluder(PageData pageData) {
        this.pageData = pageData;
        testPage = pageData.getWikiPage();
        pageCrawler = testPage.getPageCrawler();
        newPageContent = new StringBuffer();
    }

    private String render(boolean isSuite) throws Exception {
        this.isSuite = isSuite;
        if(isTestPage()) {
            includeSetupAndTeardownPages();
        }
        return pageData.getHtml();
    }

    private boolean isTestPage() throws Exception {
        return pageData.hasAttribute("Test");
    }

    private void includeSetupAndTeardownPages() throws Exception {
        includeSetupPages();
        includePageContent();
        includeTeardownPages();
        updatePageContent();
    }

    private void includeSetupPages() throws Exception {
        if(isSuite) {
            includeSuiteSetupPage();
        }
        includeSetupPage();
    }

    private void includeSuiteSetupPage() throws Exception {
        include(SuiteResponder.SUITE_SETUP_NAME, "-setup");
    }

    private void includeSetupPage() throws Exception {
        include("SetUp", "-setup");
    }

    private void includePageContent() throws Exception {
        newPageContent.append(pageData.getContent());
    }

    private void includeTeardownPages() throws Exception {
        includeTeardownPage();
        if(isSuite)
            includeSuiteTeardownPage();
    }

    private void includeTeardownPage() throws Exception {
        include("TearDown", "-teardown");
    }

    private void includeSuiteTeardownPage() throws Exception {
        include(SuiteResponder.SUITE_TEARDOWN_NAME, "-teardown");
    }

    private void updatePageContent() throws Exception {
        pageData.setContent(newPageContent.toString());
    }

    private void include(String pageName, String arg) throws Exception {
        WikiPage inheritedPage = findInheritedPage(pageName);
        if(inheritedPage != null) {
            String pathPathName = getPathNameForPage(inheritedPage);
            buildIncludeDirective(pagePathName, arg);
        }
    }

    private WikiPage findInheritedPage(String pageName) throws Exception {
        return PageCrawlerImpl.getInheritedPage(pageName, testPage);
    }

    private String getPathNameForPage(WikiPage page) throws Exception {
        WikiPagePath pagePath = pageCrawel.getFullPath(page);
        return PathParser.render(pagePath);
    }

    private void buildIncludeDirective(String pagePathName, String arg) {
        newPageContent
            .append("\n!include ")
            .append(arg)
            .append(" .")
            .append(pathPathName)
            .append("\n");
    }
}
```
