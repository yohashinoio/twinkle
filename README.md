<div align="center">
    <h1>The Miko Programming Language</h1>
</div>

- [Operators](#operators)
  - [Arithmetic operators](#arithmetic-operators)
  - [Comparison operators / Relational operators](#comparison-operators--relational-operators)
  - [Assignment operators](#assignment-operators)
  - [Other operators](#other-operators)
- [Data types](#data-types)
  - [Integer types](#integer-types)
  - [Boolean type](#boolean-type)
  - [Void type](#void-type)
  - [Pointer types](#pointer-types)
- [Comments](#comments)
  - [Single line comment](#single-line-comment)
  - [Multi line comment](#multi-line-comment)
- [Declaration and Definition](#declaration-and-definition)
  - [Function](#function)
  - [Variable](#variable)
- [Linkage](#linkage)
  - [Function](#function-1)
- [Statements](#statements)
  - [Expression statement](#expression-statement)
  - [Compound statement](#compound-statement)
  - [Selection statement](#selection-statement)
  - [Iteration statement](#iteration-statement)
  - [Jump statement](#jump-statement)
- [Example](#example)
  - [Hello world](#hello-world)
  - [First 40 fibonacci numbers](#first-40-fibonacci-numbers)
- [References](#references)
- [License](#license)

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
| Operator name     | Syntax |
| ----------------- | ------ |
| Direct assignment | a = b  |

### Other operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |
| Conversion    | a as type |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## Data types
### Integer types
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

### Boolean type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |
```rust
var f: bool;
```

### Void type
| Name |
| ---- |
| void |
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
```rust
extern puts(s: *i8) -> i32;

var p: *i32;
var s: *i8 = "hello, world";
```

## Comments
### Single line comment
```rust
func main() -> i32
{
  // This is a comment.
  ret 0;
}
```

### Multi line comment
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

## Declaration and Definition
### Function
```rust
extern twice(n: i32) -> i32; // Declaration

func main() -> i32 // Definition
{
  ret twice(58);
}

func twice(n: i32) -> i32 // Definition
{
  ret n * 2;
}
```
```rust
func f(n: i32) -> i32
{
  n = 123; // NG
  ret n;
}

func g(mut n: i32) -> i32
{
  n = 123; // OK
  ret n;
}
```
### Variable
```rust
func main() -> i32
{
  var i: i32; // OK. Constant. The value that originally existed in memory is stored.
  i = 58; // NG

  var j: i32 = 48; // OK. Constant.
  j = 58; // NG

  var mut k: i32 = 48; // OK. Mutable.
  k = 58; // OK
}
```

## Linkage
### Function
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

## Statements
### Expression statement
```peg
expression? ';'
```

### Compound statement
```peg
'{' *statement '}'
```

### Selection statement
```peg
"if" '(' condition ')'
  (compound-statement | statement)
```

### Iteration statement
```peg
"for" '(' expression? ';' condition? ';' expression? ')'
  (compound-statement | statement)
```

### Jump statement
```peg
"ret" expression ';'
```

## Example
### Hello world
```rust
extern puts(s: *i8) -> i32;

func main() -> i32
{
  puts("hello, world");
}
```
### First 40 fibonacci numbers
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
