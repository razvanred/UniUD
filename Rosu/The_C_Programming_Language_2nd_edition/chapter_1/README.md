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

