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
  - [Fibonacci number](#fibonacci-number)
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
| 128-bit | i128   | u128     |
```rust
func main() -> i32
{
  let mutable a: i8 = 2147483647; // Max
  a = -2147483648; // Min

  let mutable b: u8 = 4294967295; // Max
  b = 0; // Min
}
```

### Boolean type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |
```rust
func main() -> i32
{
  let f: bool = true;
  let g: bool = false;
}
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
extern twice(n) -> i32; // Declaration

func main() -> i32 // Definition
{
  ret twice(58);
}

func twice(n) -> i32// Definition
{
  ret n * 2;
}
```
### Variable
```rust
func main() -> i32
{
  let i; // OK. Constant. The value that originally existed in memory is stored.
  i = 58; // NG

  let j = 48; // OK. Constant.
  j = 58; // NG

  let mutable k = 48; // OK. Mutable.
  k = 58; // OK
}
```

## Linkage
### Function
```rust
func twice(n) -> i32 // External linkage
{
  ret n * 2;
}

func private thrice(n) -> i32 // Internal linkage
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
### Fibonacci number
```rust
func fib(n) -> i32
{
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);

  ret 0; // A return is always required
         // at the end of any function other than the main function.
}

func main() -> i32
{
  let result = fib(40);
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
