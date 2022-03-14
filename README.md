<div align="center">
    <h1>The Miko Programming Language</h1>
</div>

- [Operators](#operators)
  - [Arithmetic Operators](#arithmetic-operators)
  - [Comparison Operators/Relational Operators](#comparison-operatorsrelational-operators)
  - [Assignment Operators](#assignment-operators)
  - [Other Operators](#other-operators)
- [Comments](#comments)
  - [Single Line Comment](#single-line-comment)
  - [Block Comment](#block-comment)
- [Declaration And Definition](#declaration-and-definition)
  - [Function](#function)
  - [Variable](#variable)
- [Linkage](#linkage)
  - [Function](#function-1)
- [Statements](#statements)
  - [Expression Statement](#expression-statement)
  - [Compound Statement](#compound-statement)
  - [Selection Statement](#selection-statement)
  - [Iteration Statement](#iteration-statement)
  - [Jump Statement](#jump-statement)
- [Example](#example)
  - [Fibonacci Number](#fibonacci-number)
- [References](#references)
- [License](#license)

## Operators
### Arithmetic Operators
| Operator name  | Syntax |
| -------------- | ------ |
| Addition       | a + b  |
| Subtraction    | a - b  |
| Multiplication | a * b  |
| Division       | a / b  |
| Unary plus     | +a     |
| Unary minus    | -a     |

### Comparison Operators/Relational Operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b |

### Assignment Operators
| Operator name     | Syntax |
| ----------------- | ------ |
| Direct assignment | a = b  |

### Other Operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## Comments
### Single Line Comment
```rust
func main()
{
  // This is a comment.
  ret 0;
}
```

### Block Comment
```rust
func main()
{
  /*
  The only ones who can shoot are those who are prepared to be shot.
  ― Lelouch Vi Britannia
  */
  ret 0;
}
```

## Declaration And Definition
### Function
```rust
extern twice(n); // Declaration

func main() // Definition
{
  ret f(58);
}

func twice(n) // Definition
{
  ret n * 2;
}
```
### Variable
```rust
func main()
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
func twice(n) // External linkage
{
  ret n * 2;
}

func private thrice(n) // Internal linkage
{
  ret n * 3;
}
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
### Fibonacci Number
```rust
func fib(n)
{
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);

  ret 0;
}

func main()
{
  let result = fib(40);
}
```

## References
- [X3 Documentation](http://ciere.com/cppnow15/x3_docs/): Documentation for Boost.Spirit.X3.

- [Using X3 Slides](http://ciere.com/cppnow15/x3_docs/): Slides for Boost.Spirit.X3.

- [LLVM Tutorial](https://llvm.org/docs/GettingStartedTutorials.html): Tutorials about using LLVM. Includes a tutorial about making a custom language with LLVM.

- [きつねさんでもわかるLLVM](https://tatsu-zine.com/books/llvm): あらゆる可能性を秘めたコンパイラ基盤として注目されているLLVM。本書はコンパイラを実際に作りながらLLVMのフロントエンドからバックエンドまでを幅広く解説した世界初(!?)のLLVM解説本です。

Thank you!

## License
This project is available under the Apache-2.0 license.<br/>
See LICENSE for the full content of the licenses.
