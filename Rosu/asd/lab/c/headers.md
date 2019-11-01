# Header file

A header file is a file with extension ```.h``` which contains C function declarations and macro definitions to be shared between several source files.

There are two types of header files:

* The file that the programmer writes;
* The file that comes with your compiler.

You request to use a header file in your program by including it with the C preprocessive directive ```#include```, like you have seen inclusion of ```stdio.h``` header file.

Including a header file is equal to copying the content of the header file but we do not do it because it will be error-prone and it is not a good idea to copy the content of a header file in the source files, especially if we have multiple source files in a program.

A simple practice in C or C++ programs is that we keep all the constants, macros, system wide global variables, and function prototypes in the header files and include that header file wherever it is required.

## Include Syntax

Both the user and the system header files are included using the preprocessing directive ```#include```. It has the fowllowing two forms:

* ```c
  #include <file>
  ```

  This form is used for system header files. It searches for a file named _file_ in a standard list of system directories.

* ```c
  #include "file"
  ```

  This form is used for header files of your own program. It searches for a file named _file_ in the directory containing the current file.

In both forms, you can prepend directories to the respective search list with the ```-l``` option while compiling your source code.

## Include Operation

```h
// header.h
char *test(void);
```

Before compilation:

```c
// main.c
#include <stdio.h>
#include "header.h"

int main(void) {
    puts(test());
}
```

After compilation:

```c
//main.c
# include <stdio.h>

char *test(void);

int main(void) {
    puts(test());
}
```

## Once-Only Headers

If a header file happens to be included twice, the compiler will process its contents twice and it will result in an error. The standard way to prevent this is to enclose the entire real contents of the file in a conditional, like this -

```h
#ifndef HEADER_FILE
#define HEADER_FILE

// the entire header file file

#endif
```

This construct is commonly known as a wrapper ```#ifndef```. When the header is included again, the conditional will be false, because HEADER_FILE is defined. The preprocessor will skip over the entire contents of the file, and the compiler will not see it twice.

## Computed Includes

Sometimes it is necessary to select one of the several different header files to be included into your program.

```h
#if SYSTEM_1
    #include "system_1.h"
#elif SYSTEM_2
    #include "system_2.h"
#elif SYSTEM_3
    ...
#endif
```

But as it grows, it becomes tedious, instead the preprocessor offers the ability to use a macro for the header name. This is called a **computed include**. Instead of wrinting a header name as the direct argument of ```#include```, you simply put a macro name there -

```h
̀#define SYSTEM_H "system_1.h"
...
#include SYSTEM_H
```

```SYSTEM_H``` will be expanded, and the preprocessor will look for ```system_1.h``` as if the ```#include``` had been written that way originally. SYSTEM_H could be defined by your Makefile with a -D option.
