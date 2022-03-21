<div align="center">
    <h1>The Miko Programming Language</h1>
</div>

- [Getting Started](#getting-started)
  - [Hello World](#hello-world)
  - [AOT Compile](#aot-compile)
  - [JIT Compile](#jit-compile)
  - [More Compiler Options](#more-compiler-options)
- [Operators](#operators)
  - [Arithmetic Operators](#arithmetic-operators)
  - [Comparison Operators / Relational Operators](#comparison-operators--relational-operators)
  - [Assignment Operators](#assignment-operators)
  - [Other Operators](#other-operators)
- [Fundamental (built-in) Types](#fundamental-built-in-types)
  - [Integer Types](#integer-types)
  - [The boolean Type](#the-boolean-type)
  - [The void type](#the-void-type)
  - [Pointer types](#pointer-types)
- [Comments](#comments)
  - [Single Line Comment](#single-line-comment)
  - [Multi Line Comment](#multi-line-comment)
- [Functions](#functions)
  - [Declaration](#declaration)
  - [Definition](#definition)
  - [Mutable parameters](#mutable-parameters)
  - [Linkage](#linkage)
- [Variables](#variables)
  - [Definition](#definition-1)
  - [Mutable variables](#mutable-variables)
- [Statements](#statements)
  - [Expression Statement](#expression-statement)
  - [Compound Statement](#compound-statement)
  - [Selection Statement](#selection-statement)
  - [Iteration Statement](#iteration-statement)
  - [Jump Statement](#jump-statement)
- [Example](#example)
  - [First 40 Fibonacci Numbers](#first-40-fibonacci-numbers)
- [References](#references)
- [License](#license)

## Getting Started
### Hello World
```rust
// hello.txt
extern puts(s: *i8) -> i32;

func main() -> i32
{
  puts("hello, world");
}
```

### AOT Compile
```bash
$ mikoc hello.txt
$ cc hello.o
$ ./a.out
hello, world
```

### JIT Compile
```bash
$ mikoc --jit hello.txt
hello, world
```

### More Compiler Options
Please see help.
```bash
$ mikoc --help
```

## Operators
### Arithmetic Operators
| Operator name  | Syntax |
| -------------- | ------ |
| Addition       | a + b  |
| Subtraction    | a - b  |
| Multiplication | a * b  |
| Division       | a / b  |
| Modulo         | a % b  |
| Unary plus     | +a     |
| Unary minus    | -a     |

### Comparison Operators / Relational Operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b  |

### Assignment Operators
| Operator name     | Syntax |
| ----------------- | ------ |
| Direct assignment | a = b  |

### Other Operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |
| Conversion    | a as type |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## Fundamental (built-in) Types
### Integer Types
| Length  | Signed | Unsigned |
| ------- | ------ | -------- |
| 8-bit   | i8     | u8       |
| 16-bit  | i16    | u16      |
| 32-bit  | i32    | u32      |
| 64-bit  | i64    | u64      |
```rust
var n: i32;
var m: u32;
```

### The boolean Type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |
```rust
var f: bool;
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
var p: *i32;
var s: *i8 = "hello, world";
```

## Comments
### Single Line Comment
```rust
func main() -> i32
{
  // This is a comment.
  ret 0;
}
```

### Multi Line Comment
```rust
func main() -> i32
{
  /*
  The only ones who can shoot are those who are prepared to be shot.
  ― Lelouch Vi Britannia
  */
  ret 0;
}
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
In this language, parameters are constant by default.<br/>
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
var a: i32;

var b: i32 = 48;
```

### Mutable variables
In this language, variables are constant by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
var i: i32 = 48; // Constant.
i = 58; // Error!

var mut j: i32 = 48; // Mutable.
j = 58; // OK

var mut k: i32; // Mutable.
k = 4810; // OK
```


## Statements
### Expression Statement
```peg
expression? ';'
```

### Compound Statement
```peg
'{' *statement '}'
```

### Selection Statement
```peg
"if" '(' condition ')'
  (compound-statement | statement)
```

### Iteration Statement
```peg
"for" '(' expression? ';' condition? ';' expression? ')'
  (compound-statement | statement)
```

### Jump Statement
```peg
"ret" expression ';'
```

## Example
### First 40 Fibonacci Numbers
```rust
extern putchar(ch: i32) -> i32;

func printi(n: i32) -> void
{
  if (n != 0) {
    printi(n / 10);
    putchar(48 /* '0' */ + n % 10);
  }
}

func puti(n: i32) -> void
{
  printi(n);
  putchar(10); // '\n'
}

func fib(n: i32) -> i32
{
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);
  ret 0;
}

func main() -> i32
{
  var mut i: i32;
  for (i = 1; i <= 40; i = i + 1)
    puti(fib(i));
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
