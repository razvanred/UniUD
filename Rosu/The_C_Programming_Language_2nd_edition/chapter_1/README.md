# Chapter 1: A Tutorial Introduction

## Hello, world

```hello.c```:

```c
#include <stdio.h>

main() {
    printf("hello, world\n");
}
```

A C program consists of:

- **_functions_**, that contains _statements_ that specify the computing operations to be done;
- **_variables_**, that store values used during computation.

Normally, you are at liberty to give functions whatever names you like, but ```main``` is special - your program begins executing at the beginning of ```main```. This means that every program must have a ```main``` somewhere.

```main``` will usually call other functions to help perform its job, some that you wrote, and others from libraries that are provided for you. The first line of the program,

```c
#include <stdio.h>
```

tells the compiler to include information about the standard input/output library.

One method of communicating data between functions is for the calling function to provide a list of values, called **_arguments_**, to the function it calls.

The statements of a function are enclosed in braces ```{ }```. The function main contains only one statement:

```c
printf("hello, world\n");
```

A function is called by naming it, followed by a parenthesized list of arguments.

```printf``` is a library function that prints output, in this case the string of characters between the quotes.

A sequence of characters in double quotes, like ```"hello, world\n``` is called a _character string_ or _string constant_.

## Variables and Arithmetic Expressions

The next program uses the formula ```°C = (5/9)(°F-32)``` to print the following table of Fahrenheit temperatures and their centigrade or Celsius equivalents:

| °F  | °C  |
|-----|-----|
| 0   | -17 |
| 20  | -6  |
| 40  | 4   |
| 60  | 15  |
| 80  | 26  |
| 100 | 37  |
| 120 | 48  |
| 140 | 60  |
| 160 | 71  |
| 180 | 82  |
| 200 | 93  |
| 220 | 104 |

```fahrenheit_celsius.c```:

```c
#include <stdio.h> // include information about standard library

/*
 * print Fahrenheit-Celsius table
 * for fahr = 0. 20, ..., 220
 */

main() { // define a function named main that receives no argument values
    int fahrenheit, celsius;
    int lower, upper, step;

    lower = 0;
    upper = 220;
    step = 20;

    fahrenheit = lower;
    while(fahrenheit <= upper) {
        celsius = 5 * (fahrenheit - 32) / 9;
        printf("%d\t%d\n", fahrenheit, celsius);
        fahrenheit += step;
    }
}
```

In C, all variables must be declared before they are used, usually at the beginning of the function before any executable statements. A _declaration_ announces the properties of variables; it consists of a type name and a list of variables, such as:

```c
int fahrenheit, celsius;
int lower, upper, step;
```

The type ```int``` means that the variables listed are integers, by contrast with ```float```, which means floating point, i.e., numbers that may have fractional part. The range of both ```int``` and ```float``` depends on the machine you are using; 16-bit ```int```s, which lie between -32768 and +32767, are common, as are 32-bit ```int```s. A ```float``` number is typically a 32-bit quantity, with at least 6 significant digits and magnitude generally between about 10<sup>-38</sup> and 10<sup>+38</sup>. C provides several other basic data types besides ```int``` and ```float```, including:

- ```char```: character - a single byte
- ```short```: short integer
- ```long```: long integer
- ```double```: double-precision floating point

The size of these objects are also machine-dependent. There are also **_arrays_**, **_structures_**, and **_unions_** of these basic types, **_pointers_** to them, and **_functions_** that return them.

The reason for multiplying by 5 and then dividing by 9 instead of just multiplying by 5/9 is that in C, as in many other languages, integer division _truncates_: any fractional part is discarded. Since 5 and 9 are integers, 5/9 would be truncated to zero and so all the Celsius temperatures would be reported as zero.

This example also shows a bit more of how ```printf``` works. ```printf``` is a general-purpose output formatting function. Its first argument is a string of characters to be printed, with each ```%``` indicating where one one of the other (second, third, ...) arguments is to be substituted, and in what form it is to be printed.

```printf``` is not part of the C language; there is no input or output defined in C itself. ```printf``` is just a useful function from the standard library of functions that are normally accessible to C programs. The behavior of ```printf``` is defined in the ANSI standard, however, so its properties should be the same with any compiler and library that conforms to the standard.

There are a couple of problems with the temperature conversion program. The simpler one is that the output isn't very pretty because the numbers are not right-justified. If we augment each ```%d``` in the ```printf``` statement with a width, the numbers printed will be right-justified in their fields.

For instance, we might say

```c
printf("%3d %6d\n", fahrenheit, celsius);
```

to print the first number of each line in a field three digits wide, and the second in a field six digits wide.

The more serious problem is that because we have used integer arithmetic, the Celsius temperatures are not very accurate.

Second version of ```fahrenheit_celsius.c```:

```c
#include <stdio.h> // include information about standard library

/*
 * print Fahrenheit-Celsius table
 * for fahr = 0. 20, ..., 220
 */

main() { // define a function named main that receives no argument values
    float celsius;
    int fahrenheit;
    int lower, upper, step;

    lower = 0;
    upper = 220;
    step = 20;

    fahrenheit = lower;
    while(fahrenheit <= upper) {
        celsius = (5.0f / 9.0f) * (float) (fahrenheit - 32);
        printf("%3d\t%6.1f\n", fahrenheit, celsius);
        fahrenheit += step;
    }
}
```

```%6.1f``` describes a number (celsius) that is to be printed at least six characters wide, with 1 digit after the decimal point.

```printf``` also recognizes:

| Characters | Interpretation   |
|------------|------------------|
| ```%d```   | Integer          |
| ```%f```   | Floating point   |
| ```%o```   | Octal            |
| ```%x```   | Hexadecimal      |
| ```%c```   | Character        |
| ```%s```   | Character String |
| ```%%```   | % itself         |

## The ```for``` statement

```c
#include <stdio.h>
main() {
    int fahrenheit;
    for(fahrenheit = 0; fahrenheit <= 220; fahrenheit += 20) {
        printf("%3d %6.1f\n", fahrenheit, (5.f/9.f) * (float) (fahrenheit - 32));
    }
}
```

This program produces the same answer. The ```for``` loop has 3 components:

- The *initialization*: ```fahrenheit = 0``` is done one, before the loop proper is entered.
- The *test condition* that controls the loop: when ```fahrenheit <= 220``` is ```false``` the loop terminates.
- The *increment step*: ```fahrenheit += 20```.

## Symbolic Constants

It's bad practice to bury "magic numbers" like 220 and 20 in a program; they convey little information to someone who might have to read the program later, and they are hard to change in a systematic way. One way to deal with magic numbers is to give them meaningful names. A ```#define``` line defines a _symbolic name_ or _symbolic constant_ to be a particular string of characters:

```c
#define name replacement_text
```

Thereafter, any occurence of _name_ will be replaced by the corresponding _replacement text_. The *name* has the same form as a variable name: a sequence of letters and digits that begins with a letter. The _replacement text_ can be any sequence of characters; it is not limited to numbers;

```c
#include <stdio.h>

#define LOWER 0 /* lower limit of table */
#define UPPER 220 /* upper limit */
#define STEP 20 /* step size */

float from_fahrenheit_to_celsius(int fahrenheit);

void print_temperature(int fahrenheit, float celsius);

int main() {

    for (int fahrenheit = UPPER; fahrenheit >= LOWER; fahrenheit -= STEP) {
        print_temperature(fahrenheit, from_fahrenheit_to_celsius(fahrenheit));
    }

    return 0;
}

float from_fahrenheit_to_celsius(int fahrenheit) {
    return (5.f / 9.f) * (float) (fahrenheit - 32);
}

void print_temperature(int fahrenheit, float celsius) {
    printf("%3d\t%6.1f\n", fahrenheit, celsius);
}
```

Symbolic constact names are conventionally written in upper case so they can be readily distinguished from lower case variable names. There is no semicolon at the end of a ```#define``` line.

## Character Input and Output

Text input or output, regardless of where it originates or where it goes to, is dealt with as stream of characters. A _text stream_ is a sequence of characters divided into lines; each line consists of zero or more characters followed by a newline character. It is the responsability of the library to make each input or output stream comform to this model; the C programmer using the library need not worry about how lines are represented outside the program.

The standard library provides several functions for reading or writing one character at a time, of which ```getchar``` and ```putchar``` are the simplest. Each time it is called, ```getchar``` reads the _next input character_ from a text stream and returns that as its value. That is, after

```c
c = getchar();
```

the variable ```c``` contains the next character of input. The characters normally come from keyboard; input from files is discussed in [Chapter 7](../chapter_7/README.md).

The function ```putchar``` prints a character each time it is called:

```putchar(c)```

prints the contents of the integer variable ```c``` as a character, usually on the screen. Calls to ```putchar``` and ```printf``` may be interleaved; the output will appear in the order in which the calls are made.

### File Copying

Program that copies its input to its output one character at a time:

_read a character_
while (_character is not end-of-file indicator_) {
    _output the character just read_
    _read a character_
}

Converting this into C gives us:

```c
#include <stdio.h>

main() {
    int c;
    c = getchar();
    while (c != EOF) {
        putchar(c);
        c = getchar();
    }
}
```

The single characters internally are stored just as different bit patterns. The type ```char``` is specifically meant for storing such character data, but any integer type can be used.

The problem is distinguishing the end of the input from valid data. The solution is that ```getchar``` returns a distinctive value when there is no more input, a value that cannot be confused with any real character. This value is called ```EOF```, for "end of file".

```EOF``` is an integer defined in ```stdio.h```. By using the symbolic constant, we are assured that nothing in the program depends on the specific numeric value.

In C, any assignment, such as

```c
c = getchar();
```

is an expression and has a value, which, is the value of the left hand side after the assignment. This means that an assignment can appear as part of a larger expression:

```c
#include <stdio.h>

main() {
    int c;
    while ((c = getchar()) != EOF)
        putchar(c);
}
```

The parantheses around the assignment within the condition are necessary. The _precedence_ of ```!=``` is higher than that of ```=```, which means that in the absence of parentheses the relation test ```!=``` would be done before the assignment ```=```.

### Character Counting

```c
#include <stdio.h>

main() {
    long i;

    for(i = 0; getchar() != EOF; i++)
        ;

    printf("characters inserted: %ld", i);
}
```

The character counting program accumulates its count in a ```long``` variable instead of an ```int```. ```long``` integers are at least 32 bits. Although on some machines, ```int``` and ```long``` are the same size, on others an ```int``` is 16 bits, with an maximum value of 32767, and it would take relatively little input to overflow an ```int``` counter.

It may be possible to cope with even bigger numbers by sing a ```double``` (double precision ```float```).

```c
#include <stdio.h>

main() {
    double i;

    for(i = 0; getchar() != EOF; i++)
        ;

    printf("%.0f\n", i);
}
```

```printf``` uses ```%f``` for both ```float``` and ```double```.

The body of this ```for``` loop is empty because all of the work is done in the test and increment parts. But the grammatical rules of C require that a ```for``` statement have a body. The isolated semicolon, called a **_null statement_**, is there to satisfy that requirement.

### Line Counting

As mentioned before, the standard library encures that an input text stream appears as a sequence of lines, each terminated by a newline. Hence, counting lines is just counting newlines:

```c
#include <stdio.h>

int main() {
    int c;
    long lines = 0;

    while((c = getchar()) != EOF) {
        if(c == '\n') {
            lines++;
        }
    }

    printf("number of lines: %ld\n", lines);

    return 0;
}
```

A character written between single quotes represents an integer value equal to the numerical value of the character in the machine's character set. This is called a _character constrant_, although it is just another way to write a small integer. Of course, '\n' is to be preferred over 10:its meaning is obious, and it is independent of a particular character set.

Note that "\n" is a string constant that happens to contain only one character.

### Word Counting

A word is any sequence of characters that does not contain a blank, tab or newline.

```c
#include <stdio.h>

#define IN 1 /* inside word */
#define OUT 0 /* outside word */

int main() {

    int c, state;
    long words, characters, lines;

    state = OUT;
    words = characters = lines = 0;

    while((c = getchar()) != EOF) {
        characters++;

        if(c == '\n') {
            lines++;
        }

        if(c == '\n' || c == '\t' || c == ' ') {
            state = OUT;
        } else if(state == OUT) {
            state = IN;
            words++;
        }
    }

    printf("%ld %ld %ld\n", lines, words, characters);

    return 0;
}
```

## Arrays

Write a program to count the number of occurrences of each digit, of white space characters (blank, tab, newline), and of all other characters.

There are twelve categories of input, so it's convenient to use an array to hold the number of occurences of each digit, rather than ten individual variables.

```c
#include <stdio.h>

#define NUMBER_OF_DIGITS 10

int is_not_EOF(int c);

int is_digit(int c);

int is_white_space(int c);

int main() {

    int c;
    long white_spaces, others;
    long digits[NUMBER_OF_DIGITS];

    white_spaces = others = 0;

    for (int i = 0; i < NUMBER_OF_DIGITS; i++) {
        digits[i] = 0;
    }

    while (is_not_EOF(c = getchar())) {
        if (is_digit(c)) {
            digits[c - '0']++;
        } else if (is_white_space(c)) {
            white_spaces++;
        } else {
            others++;
        }
    }

    printf("digits:");
    for (int i = 0; i < NUMBER_OF_DIGITS; i++)
        printf(" %ld", digits[i]);

    printf("\nwhite spaces: %ld\nothers: %ld\n", white_spaces, others);

    return 0;
}

int is_not_EOF(const int c) {
    return c != EOF;
}

int is_digit(int c) {
    return c >= '0' && c <= '9';
}

int is_white_space(int c) {
    return c == '\t' || c == ' ' || c == '\n';
}
```

By definition, ```char```s are just small integers, so ```char``` variables and constants are identical to ```int```s in arithmentic expressions.

## Functions

A function provides a convenient way to encapsulate some computation, which can then be used without worring about its implementation. With properly designed functions, it is possible to ignore **how** a job is done; knowing **what** is done is sufficient.

```c
#include <stdio.h>

double positive_power(double base, double exponent);

int main() {
    printf("%.0f\n", positive_power(5, -1));
    return 0;
}

double positive_power(double base, double exponent) {
    if (exponent < 0) {
        perror("The exponent should be equal to or greater than 0");
        return -1;
    }

    if (exponent == 0) {
        return 1;
    }

    return base * positive_power(base, exponent - 1);
}
```

Functions definitions can appear in any order, and in one source file or several, although no function can be split between files. If the source program appears in several files, you may have to say more to compile and load it than if it all appears in one, but that is an operating system matter, not a language attribute.

```c
int power(int base, int n) { ... }
```

The names used by ```power``` for its parameters are local to ```power```, and are not visible to any other function: other routines can use the same names without conflict.

- **Parameter**: variable named in the paranethesized list in a function definition;
- **Argument**: value used in a call of the function.

The declaration

```c
int power(int m, int n);
```

just before ```main``` says that ```power``` is a function that expects two ```int``` arguments and returns an ```int```. This declaration, which is called a **function prototype**, has to agree with the definition and uses of ```power```. It is an error if the definition of a function or any uses of it do not agree with its prototype.

Parameters names need not agree. Indeed, parameter names are optional in a function prototype, so for the prototype we could have written:

```c
int power(int, int);
```

Well-chosen namesa are good documentation

## Arguments - Call by Value

In C, all function arguments are passed by value. This means that the called function is given the values of its arguments in temporary variables rather than the originals. This leads to some different properties than are seen with *call by reference* languages like Fortran or with ```var``` parameters in Pascal, in which the called routine has access to the original argument, not a local copy.

The main distinction is that in C the called function cannot directly alter a variable in the calling function; it can only alter its private, temporary copy.

Call by value is an asset, however, not a liability. It usually leads to more compact programs with fewer extraneous variables, because parameters can be treated as conveniently initialized local variables in the called routine. For example, here's a version of ```power``` that makes use of this property.

```c
int power(int base, int exponent) {
    int p;

    for(p = 1; exponent > 0; exponent--) {
        p *= base;
    }

    return p;
}
```

The parameter ```exponent``` is used as a temporary vairable, and is counted down (a ```for``` loop that runs backwards) until it becomes zero; there is no longer a need for the variable ```i```. Whatever is done to ```exponent``` inside ```power``` has no effect on the argument that ```power``` was originally called with.

When necessary, it's possible to arange for a function to modify a variable in a calling routine. The caller must provide the **address** of the variable to be set (technically a *pointer* to the variable), and the called function must declare the parameter to be pointer and access the variable indirectly through it.

The story is different for arrays. When the name of an array is used as an argument, the value passed to the function is the location or address of the beginning of the aray = there is no copying of array elements. By subscripting this value, the function can access and alter any element of the array.

## Character Arrays

The most common type of array in C is the array of characters.

Write a program that reads a set of text lines and prints the longest.

while(_there's another line_) {
    if(_it's longer than the previous longest_) {
        _save it_
        _save its length_
    }
}
_print longest line_

Since things divide so nicely, it would be well to write them that way too.

```c
#include <stdio.h>

#define MAX_LINE_SIZE 1000

int get_line(char line[], int max_line_size);

void copy(char to[], const char from[]);

int is_not_EOF(int character);

int is_not_newline(int character);

int is_newline(int character);

int is_terminator(int character);

void put_terminator(char to[], int length);

int main() {
    int current_length;
    int max_length;
    char line[MAX_LINE_SIZE];
    char longest[MAX_LINE_SIZE];

    max_length = 0;

    while ((current_length = get_line(line, MAX_LINE_SIZE)) > 0) {
        if (current_length > max_length) {
            max_length = current_length;
            copy(longest, line);
        }
    }

    if (max_length > 0) {
        printf("%s", longest);
    }

    return 0;
}

int get_line(char line[], int max_line_size) {
    int c, length;

    for (length = 0; length < max_line_size - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++) {
        line[length] = (char) c;
    }

    if (is_newline(c)) {
        line[length++] = (char) c;
    }

    put_terminator(line, length);

    return length;
}

void put_terminator(char to[], const int length) {
    to[length] = '\0';
}

int is_not_newline(const int c) {
    return c != '\n';
}

int is_newline(const int c) {
    return c == '\n';
}

int is_not_EOF(const int c) {
    return c != EOF;
}

void copy(char to[], const char from[]) {
    for (int i = 0; !is_terminator(to[i] = from[i]); i++);
}

int is_terminator(const int c) {
    return c == '\0';
}
```

When a string constant like

```c
"hello\n"
```

appears in a C program, it is stored as an array of characters containing the characters of the string and terminated with a ```\0``` to mark the end.

The ```%s``` format specification in ```printf``` expectes the corresponding argument to be a string represented in this form. ```copy``` laso relies on the fact that its input argument is terminated by ```\0```, and, it copies this character into the output argument. (All of this implies that ```\0``` is not a part of normal text.)

## External Variables and Scope

The variables in ```main``` are private or local to main. Because they are declared within ```main```, no other function can have direct access to them. The same is true of the variables declared in other functions. Each local variable in a function comes into existence only when the function is called, and disappears when the function is exited. This is why such variables are usually known as **_automatic variables_**, following terminology in other languages. ([Chapter 4](../chapter_4/README.md) discusses the ```static``` storage class, in which local variables do retain their values between calls.)

Because automatic variables come and go with function invocation, they do not retain their values from one call to the next, and must be explicitly set upon each entry. If they are not set, they will contain garbage.

As an alternative to automatic variables, it is possible to define variables that are external to all functions, that is, variables that can be accessed by name by any function. Because external variables are globally accessible, they can be used instead of argument lists to communicate data between functions.

An external variable must be **defined**, exactly once, outside of any function. The variable must also be **declared** in each function that want to access it; this states the type of the variable. The **declaration** may be an explicit ```extern``` statement or may be implicit from context.

```c
#include <stdio.h>

#define MAX_LENGTH 1000

int length_longest_line;
char current_line[MAX_LENGTH];
char longest_line[MAX_LENGTH];

int get_line(void);

void copy(void);

void put_terminator(char to[], int length);

int is_newline(int character);

int is_EOF(int character);

int is_not_newline(int character);

int is_not_EOF(int character);

int is_not_terminator(int character);

int main() {
    int length_current_line;

    while ((length_current_line = get_line()) > 0) {
        if (length_current_line > length_longest_line) {
            length_longest_line = length_current_line;
            copy();
        }
    }

    if (length_longest_line > 0) {
        printf("%s\n", longest_line);
    }

    return 0;
}

int get_line(void) {
    int c, length;
    extern char current_line[];

    for (length = 0; length < MAX_LENGTH - 1 && is_not_EOF(c = getchar()) && is_not_newline(c); length++)
        current_line[length] = (char) c;

    if (is_newline(current_line[length])) {
        current_line[length++] = (char) c;
    } else if (is_EOF(current_line[length]) && length > 0) {
        current_line[length++] = '\n';
    }

    put_terminator(current_line, length);

    return length;
}

void put_terminator(char to[], const int length) {
    to[length] = '\0';
}

int is_newline(const int c) {
    return c == '\n';
}

int is_not_newline(const int c) {
    return c != '\n';
}

int is_EOF(const int c) {
    return c == EOF;
}

int is_not_EOF(const int c) {
    return c != EOF;
}

int is_not_terminator(const int c) {
    return c != '\0';
}

void copy(void) {
    extern char longest_line[], current_line[];
    for (int i = 0; is_not_terminator(longest_line[i] = current_line[i]); i++);
}
```

Before a function can use an external variable, the variable must be made known to the function. One way to do this is to write an ```extern``` declaration in the function; the declaration is the same as before except for the added keyword ```extern```.

In certain circumnstances, the ```extern``` declaration can be omitted. If the definition of an external variable occurs in the source file before its use in a particular function, then there is no need for an ```extern``` declaration in the function.

If the program is in several source files, and a variable is defined in _file1_ and used in *file2* and *file3*, then ```extern``` declarations are needed in *file2* and *file3* to connect the occurrences of the variable. The usual practice is to collect ```extern``` declarations of variables and functions in a separate file, historically called an ***header***, that is included by ```#include``` at the front of each source file. The suffix ```.h``` is conventional for header names. The functions of the standard library, for example, are declaraed in headers like ```<stdio.h>```. This topic is discussed in [Chapter 4](../chapter_4/README.md), and the library itself in [Chapter 7](../chapter_7/README.md) and [Appendix B](../appendix_b/README.md).

### Declaration and Definition differences

- **Definition**: refers to the place where the variable is created or assigned storage;
- **Declaration**: refers to places where the nature of the variable or function is stated but no storage is allocated.

### Tips for ```external```

There is a tendency to make everything in sight an ```extern``` variable because it appears to simplify communications - arugment lists are short and variables are always there when you want them. But external variables are always there even when you don't want them. Relying too heavily on external variables if fraught with peril since it leads to programs whose data connections are not all obvious - variables can be changed in unexpected and even inadvertent ways, and the program is hard to modify.

The second version of the longest-line program is inferior to the first, partly for these reasons, and partly because it destroys the generality of two useful functions by wiring into them the names of the variables they manipulate.
