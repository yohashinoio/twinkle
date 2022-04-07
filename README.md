<div align="center">
    <h1>The Miko Programming Language</h1>
</div>

- [Getting Started](#getting-started)
  - [Install dependencies](#install-dependencies)
  - [Compile this project](#compile-this-project)
  - [Hello world](#hello-world)
  - [AOT compile](#aot-compile)
  - [JIT compile](#jit-compile)
  - [More compiler options](#more-compiler-options)
- [Operators](#operators)
  - [Arithmetic operators](#arithmetic-operators)
  - [Comparison operators / Relational operators](#comparison-operators--relational-operators)
  - [Assignment operators](#assignment-operators)
  - [Pointer operators](#pointer-operators)
  - [Other operators](#other-operators)
- [Fundamental (Built-in) Types](#fundamental-built-in-types)
  - [Integer types](#integer-types)
  - [The boolean type](#the-boolean-type)
  - [The void type](#the-void-type)
  - [Pointer types](#pointer-types)
- [Comments](#comments)
  - [Single line comment](#single-line-comment)
  - [Multi line comment](#multi-line-comment)
- [Functions](#functions)
  - [Declaration](#declaration)
  - [Definition](#definition)
  - [Mutable parameters](#mutable-parameters)
  - [Linkage](#linkage)
- [Variables](#variables)
  - [Definition](#definition-1)
  - [Mutable variables](#mutable-variables)
  - [Type inference](#type-inference)
- [Implicit conversions](#implicit-conversions)
- [Statements](#statements)
  - [Expression statements](#expression-statements)
  - [Compound Statement (Block)](#compound-statement-block)
  - [If-else statement](#if-else-statement)
  - [While statement](#while-statement)
  - [For statement](#for-statement)
  - [Break statement](#break-statement)
  - [Continue statement](#continue-statement)
  - [Return statement](#return-statement)
- [Example](#example)
  - [First 40 fibonacci numbers](#first-40-fibonacci-numbers)
- [References](#references)
- [License](#license)

## Getting Started
### Install dependencies
LLVM, Boost, CMake, Make (ninja), GCC (clang) must be installed.<br/>
Here is how to install them in ubuntu.
```bash
$ sudo apt update -y
```
Install GCC Make CMake Boost.
```bash
$ sudo apt install -y build-essential cmake libboost-all-dev
```
Install LLVM. (https://apt.llvm.org/)
```bash
$ sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
```
Please make sure you can use the llvm-config command.<br/>
If it is llvm-config-xxx, please use symbolic links, etc.<br/>
Here is an example if /usr/bin/llvm-config-xxx.
```bash
$ sudo ln -s /usr/bin/llvm-config-xxx /usr/local/bin/llvm-config
```

### Compile this project
First, clone.
```bash
$ git clone https://github.com/GothicLoli/miko.git
$ cd miko
```
Next, create a build directory.
```bash
$ mkdir build && cd $_
```
Finally, build. (It may take a few minutes)
```bash
$ cmake ..
$ sudo make install
```
If you want to specify where to install.
```bash
$ cmake .. -DCMAKE_INSTALL_PREFIX="path/to/install"
$ make install
```

### Hello world
```rust
// hello.txt
extern printf(fmt: *i8, ...) -> i32;

func main() -> i32
{
  printf("hello, world\n");
}
```

### AOT compile
```bash
$ mikoc hello.txt
$ cc -static hello.o
$ ./a.out
hello, world
```

### JIT compile
```bash
$ mikoc --jit hello.txt
hello, world
```

### More compiler options
Please see help.
```bash
$ mikoc --help
```

## Operators
### Arithmetic operators
| Operator name  | Syntax |
| -------------- | ------ |
| Addition       | a + b  |
| Subtraction    | a - b  |
| Multiplication | a * b  |
| Division       | a / b  |
| Modulo         | a % b  |
| Unary plus     | +a     |
| Unary minus    | -a     |

### Comparison operators / Relational operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b  |

### Assignment operators
| Operator name             | Syntax |
| ------------------------- | ------ |
| Direct assignment         | a = b  |
| Addition assignment	      | a += b |
| Subtraction assignment	  | a -= b |
| Multiplication assignment	| a *= b |
| Division assignment	      | a /= b |
| Modulo assignment         | a %= b |

### Pointer operators
| Operator name | Syntax |
| ------------- | ------ |
| Address-of    | &a     |
| Indirection   | *a     |

### Other operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |
| Conversion    | a as type |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## Fundamental (Built-in) Types
### Integer types
| Length  | Signed | Unsigned |
| ------- | ------ | -------- |
| 8-bit   | i8     | u8       |
| 16-bit  | i16    | u16      |
| 32-bit  | i32    | u32      |
| 64-bit  | i64    | u64      |
```rust
let n: i32 = -58;
let m: u32 =  58;
```

### The boolean type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |
```rust
let f: bool = true;
let g: bool = false;
```

### The void type
| Name |
| ---- |
| void |

The type void is a special type: you cannot declare a variable of type void,<br/>
but you can use type void for the return value of a function,<br/>
meaning that the function will not return a value.
```rust
func f() -> void
{
  ret;
}

func g() -> void
{
}

func main() -> i32
{
  f();
  g();
}
```

### Pointer types
| Syntax |
| ------ |
| *type  |

Note that the meaning is equivalent to that of a C pointer,<br/>
but the position of the * is opposite.
```rust
let p: *i8 = "hello, world";

let n: i32 = 4810;
let p_n: *i32 = &n;
```

## Comments
### Single line comment
```rust
// This is a comment.
```

### Multi line comment
```rust
/*
The only ones who can shoot are those who are prepared to be shot.
― Lelouch Vi Britannia
*/
```

## Functions
### Declaration
```rust
extern puts(s: *i8) -> i32;
```

### Definition
```rust
func twice(n: i32) -> i32
{
  ret n * 2;
}
```

### Mutable parameters
In this language, parameters are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
func f(n: i32) -> i32
{
  n = 123; // Error!
  ret n;
}

func g(mut n: i32) -> i32
{
  n = 123; // OK
  ret n;
}
```

### Linkage
```rust
func twice(n: i32) -> i32 // External linkage
{
  ret n * 2;
}

func private thrice(n: i32) -> i32 // Internal linkage
{
  ret n * 3;
}
```

## Variables
### Definition
```rust
let a: i32;

let b = 48; // Type inference from the initializer.
```

### Mutable variables
In this language, variables are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
let i = 48; // Constant.
i = 58; // Error!

let mut j = 48; // Mutable.
j = 58; // OK

let mut k: i32; // Mutable.
k = 4810; // OK
```

### Type inference
```rust
let n = 4810; // i32

let f = 1 as bool; // bool

let s = "hello, world"; // *i8
```

## Implicit conversions
In the case of numeric values, operands with smaller bit widths are converted to the larger bit width side.
```rust
let f: bool = true;

let n = f + 57; // OK! type of n is i32.
```

## Statements
### Expression statements
```rust
f();
```
```rust
48 + 10;
```

### Compound Statement (Block)
```rust
{
  f();
  48 + 10;
}
```

### If-else statement
```rust
let cond: bool = true;
if (cond) {
}
else
  ;
```

### While statement
```rust
let mut i = 0;
// Repeat 10 times.
while (i != 10) {
  i += 1;
}

while (/* Required */) // Error!
  ;
```

### For statement
```rust
// Repeat 10 times.
for (let mut i = 0; i != 10; i += 1) {
}

// infinite loop.
for (;;)
  ;
```

### Break statement
The break statement terminates execution of the nearest loop.
```rust
for (;;) {
  break;
}
```

### Continue statement
```rust
for (;;) {
  continue;
}
```

### Return statement
```
ret 48 + 10;
```

## Example
### First 40 fibonacci numbers
```rust
extern printf(fmt: *i8, ...) -> i32;

func fib(n: i32) -> i32
{
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);
}

func main() -> i32
{
  for (let mut i = 1; i <= 40; i = i + 1)
    printf("%d\n", fib(i));
}
```

## References
- [X3 Documentation](http://ciere.com/cppnow15/x3_docs/): Documentation for Boost.Spirit.X3.

- [Using X3 Slides](http://ciere.com/cppnow15/x3_docs/): Slides for Boost.Spirit.X3.

- [LLVM Tutorial](https://llvm.org/docs/GettingStartedTutorials.html): Tutorials about using LLVM. Includes a tutorial about making a custom language with LLVM.

- [きつねさんでもわかるLLVM](https://tatsu-zine.com/books/llvm): あらゆる可能性を秘めたコンパイラ基盤として注目されているLLVM。本書はコンパイラを実際に作りながらLLVMのフロントエンドからバックエンドまでを幅広く解説した世界初(!?)のLLVM解説本です。

## License
This project is available under the Apache-2.0 license.<br/>
See LICENSE for the full content of the licenses.
