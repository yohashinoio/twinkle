## Table of Contents
- [Table of Contents](#table-of-contents)
- [Comments](#comments)
  - [Single line comment](#single-line-comment)
  - [Multi line comment](#multi-line-comment)
- [Operators](#operators)
  - [Arithmetic operators](#arithmetic-operators)
  - [Comparison operators / Relational operators](#comparison-operators--relational-operators)
  - [Logical operators](#logical-operators)
  - [Assignment operators (Statement)](#assignment-operators-statement)
  - [Pointer operators](#pointer-operators)
  - [Other operators](#other-operators)
- [Fundamental (Built-in) Types](#fundamental-built-in-types)
  - [Integer types](#integer-types)
  - [The character type](#the-character-type)
  - [The boolean type](#the-boolean-type)
  - [The void type](#the-void-type)
  - [Pointer types](#pointer-types)
  - [Array types](#array-types)
- [Variables](#variables)
  - [Definition](#definition)
  - [Mutable variables](#mutable-variables)
  - [Type inference](#type-inference)
- [Implicit conversions](#implicit-conversions)
- [Functions](#functions)
  - [Declaration](#declaration)
  - [Definition](#definition-1)
  - [Mutable parameters](#mutable-parameters)
  - [Linkage](#linkage)
- [Identifier naming rules](#identifier-naming-rules)
- [Integer literal](#integer-literal)
  - [Decimal](#decimal)
  - [Octal](#octal)
  - [Hexadecimal](#hexadecimal)
  - [Binary](#binary)
- [Statements](#statements)
  - [Expression statements](#expression-statements)
  - [Compound Statement (Block)](#compound-statement-block)
  - [If-else statement](#if-else-statement)
  - [While statement](#while-statement)
  - [For statement](#for-statement)
  - [Break statement](#break-statement)
  - [Continue statement](#continue-statement)
  - [Return statement](#return-statement)

## Comments
### Single line comment
```rust
// This is a comment.
```

### Multi line comment
```rust
/*
This
is
a
comment.
*/
```

## Operators
### Arithmetic operators
| Operator name                | Syntax |
| ---------------------------- | ------ |
| Addition                     | a + b  |
| Subtraction                  | a - b  |
| Multiplication               | a * b  |
| Division                     | a / b  |
| Modulo                       | a % b  |
| Unary plus                   | +a     |
| Unary minus                  | -a     |

### Comparison operators / Relational operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b  |

### Logical operators
| Operator name | Syntax |
| ------------- | ------ |
| Logical not   | !a     |

### Assignment operators (Statement)
| Operator name             | Syntax |
| ------------------------- | ------ |
| Direct assignment         | a = b  |
| Addition assignment	      | a += b |
| Subtraction assignment	  | a -= b |
| Multiplication assignment	| a *= b |
| Division assignment	      | a /= b |
| Prefix increment          | ++a    |
| Prefix decrement          | --a    |
| No postfix increment and decrement |

In this language, **an assignment is a statement**, so you cannot use a syntax like "a = b = c".

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
let n = 58; // i32.

let n1 =  2147483647; // i32.
let n2 = -2147483648; // i32.
let n3 =  4294967295; // u32.

let n4 =  9223372036854775807; // i64
let n5 = -9223372036854775808; // i64
let n6 =  18446744073709551615; // u64
```

### The character type
| Length  | Sign     | Name |
| ------- | -------- | ---- |
| 32-bit  | Unsigned | char |

This type can have Unicode code points.<br/>
The entity is a 32-bit unsigned integer.<br/>
Passing values of this type as characters to functions such as libc will not work properly.

```rust
i32 main()
{
  let ch: char;
  let unicode = '🌸';
}
```

### The boolean type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |

```rust
i32 main()
{
  let f: bool = true;
  let g = false;
}
```

### The void type
| Name |
| ---- |
| void |

The type void is a special type: you cannot declare a variable of type void,<br/>
but you can use type void for the return value of a function,<br/>
meaning that the function will not return a value.

```rust
void f()
{
  ret;
}

void g()
{
}

i32 main()
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
i32 main()
{
  let p: *i8 = "hello, world";

  let n: i32 = 4810;
  let p_n: *i32 = &n;

  let n = 4810;
  let p_n = &n;
}
```

### Array types
| Syntax     |
| ---------- |
| type[size] |

```rust
i32 main()
{
  let a1: i32[4810];
  let a2 = {4, 8, 1, 0};
}
```

## Variables
### Definition
```rust
i32 main()
{
  let a: i32;
  let b = 48; // Type inference from the initializer.
}
```

### Mutable variables
In this language, variables are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
i32 main()
{
  let i = 48; // Constant.
  i = 58; // Error!

  let mut j = 48; // Mutable.
  j = 58; // OK

  let mut k: i32; // Mutable.
  k = 4810; // OK
}
```

### Type inference
Type inference can only be used for initializations with initializers.
```rust
i32 main()
{
  let n = 4810; // i32

  let f = 1 as bool; // bool

  let c = '💕'; // char

  let s = "hello, world"; // *i8
}
```

## Implicit conversions
In the case of numeric values, operands with smaller bit widths are converted to the larger bit width side.
```rust
i32 main()
{
  let f: bool = true;

  let n = f + 57; // OK! type of n is i32.
}
```

## Functions
Similar to C/C++ language.
### Declaration
```rust
extern i32 puts(s: *i8);
```

### Definition
```rust
i32 twice(n: i32)
{
  ret n * 2;
}
```

### Mutable parameters
In this language, parameters are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
i32 f(n: i32)
{
  n = 123; // Error!
  ret n;
}

i32 g(mut n: i32)
{
  n = 123; // OK
  ret n;
}
```

### Linkage
```rust
i32 twice(n: i32) // External linkage
{
  ret n * 2;
}

i32 private thrice(n: i32) // Internal linkage
{
  ret n * 3;
}
```

## Identifier naming rules
1. Numbers cannot be used as the first character.
1. Punctuation characters are not allowed.
1. However, '_' is allowed as an exception
1. One or more unicode graphic characters.

```rust
// 1
let 48あ10; // Error!

// 2
let ab)c; // Error!
let a,bc; // Error!

// 3
let _48あ10; // OK!

// 3
let abc; // OK!
let 💐; // OK!
let あ; // OK!
```

## Integer literal
### Decimal
Automatic promotion from i32 to u64
``` rust
let n = 4810;
```
### Octal
The maximum value for octal literals is u32.
```rust
let n = 011312;
```
### Hexadecimal
The maximum value for hexadecimal literals is u32.
```rust
let n = 0x12ca;
let m = 0x12CA;
```
### Binary
The maximum value for binary literals is u32.
```rust
let n = 0b1001011001010;
```

## Statements
### Expression statements
```rust
i32 main()
{
  f();
}
```
```rust
i32 main()
{
  48 + 10;
}
```

### Compound Statement (Block)
```rust
i32 main()
{
  {
    // Compount statement
    f();
    48 + 10;
  }

  if (true) {
    // Compount statement
  }
}
```

### If-else statement
```rust
i32 main()
{
  let cond: bool = true;

  if (cond) {
  }
  else
    ;
}
```

### While statement
```rust
i32 main()
{
  let mut i = 0;
  // Repeat 10 times.
  while (i != 10) {
    ++i;
  }

  while (/* Required */) // Error!
    ;
}
```

### For statement
```rust
i32 main()
{
  // Repeat 10 times.
  for (let mut i = 0; i != 10; ++i) {
  }

  // infinite loop.
  for (;;)
    ;
}
```

### Break statement
The break statement terminates execution of the nearest loop.
```rust
i32 main()
{
  for (;;) {
    break;
  }
}
```

### Continue statement
```rust
i32 main()
{
  for (;;) {
    continue;
  }
}
```

### Return statement
```
i32 main()
{
  ret 48 + 10;
}
```
