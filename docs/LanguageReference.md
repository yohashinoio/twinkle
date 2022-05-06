## Table of Contents
- [Table of Contents](#table-of-contents)
- [Notes](#notes)
- [Comments](#comments)
  - [Single line comment](#single-line-comment)
  - [Multi line comment](#multi-line-comment)
- [Operators](#operators)
  - [Arithmetic Operators](#arithmetic-operators)
  - [Comparison Operators / Relational Operators](#comparison-operators--relational-operators)
  - [Logical Operators](#logical-operators)
  - [Assignment Operators (Statement)](#assignment-operators-statement)
  - [Pointer Operators](#pointer-operators)
  - [Other Operators](#other-operators)
- [Fundamental (Built-in) Types](#fundamental-built-in-types)
  - [Integer Types](#integer-types)
  - [The Character Type](#the-character-type)
  - [The Boolean Type](#the-boolean-type)
  - [The Void Type](#the-void-type)
  - [Pointer Types](#pointer-types)
  - [Array Types](#array-types)
- [Variables](#variables)
  - [Definition](#definition)
  - [Mutable Variables](#mutable-variables)
  - [Type Inference](#type-inference)
- [Constant pointer](#constant-pointer)
- [Implicit Conversions](#implicit-conversions)
- [Functions](#functions)
  - [Declaration](#declaration)
  - [Definition](#definition-1)
  - [Mutable Parameters](#mutable-parameters)
  - [Linkage](#linkage)
- [Identifier Naming Rules](#identifier-naming-rules)
- [Integer Literal](#integer-literal)
  - [Decimal](#decimal)
  - [Octal](#octal)
  - [Hexadecimal](#hexadecimal)
  - [Binary](#binary)
- [Character And String Encoding](#character-and-string-encoding)
- [Array In Detail](#array-in-detail)
  - [Definition](#definition-2)
  - [Subscript](#subscript)
- [Statements](#statements)
  - [Expression Statements](#expression-statements)
  - [Compound Statement (Block)](#compound-statement-block)
  - [If-else Statement](#if-else-statement)
  - [While Statement](#while-statement)
  - [For Statement](#for-statement)
  - [Break Statement](#break-statement)
  - [Continue Statement](#continue-statement)
  - [Return Statement](#return-statement)

## Notes
Since the author only understands Japanese, this document was created with translation at DeepL.

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
### Arithmetic Operators
| Operator name                | Syntax |
| ---------------------------- | ------ |
| Addition                     | a + b  |
| Subtraction                  | a - b  |
| Multiplication               | a * b  |
| Division                     | a / b  |
| Modulo                       | a % b  |
| Unary plus                   | +a     |
| Unary minus                  | -a     |

### Comparison Operators / Relational Operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b  |

### Logical Operators
| Operator name | Syntax |
| ------------- | ------ |
| Logical NOT   | !a     |
| Logical AND   | a && b |
| Logical OR    | a || b |

### Assignment Operators (Statement)
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

### Pointer Operators
| Operator name | Syntax |
| ------------- | ------ |
| Subscript     | a[b]   |
| Address-of    | &a     |
| Indirection   | *a     |

### Other Operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |
| Conversion    | a as type |
| Sizeof        | sizeof a  |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## Fundamental (Built-in) Types
### Integer Types
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

### The Character Type
| Length  | Sign     | Name |
| ------- | -------- | ---- |
| 32-bit  | Unsigned | char |

This type can have Unicode code points.<br/>
The entity is a 32-bit unsigned integer.<br/>
Passing values of this type as characters to functions such as libc will not work properly.

```rust
func main() -> i32
{
  let ch: char;
  let unicode = '🌸';
}
```

### The Boolean Type
| Length  | Name |
| ------- | ---- |
| 1-bit   | bool |

```rust
func main() -> i32
{
  let f: bool = true;
  let g = false;
}
```

### The Void Type
| Name |
| ---- |
| void |

The type void is a special type: you cannot declare a variable of type void,<br/>
but you can use type void for the return value of a function,<br/>
meaning that the function will not return a value.<br/>
And the return type of a function is optional and has the same meaning as the void type.

```rust
func f() -> void
{
  ret;
}

// Functions returning void type do not require return.
func g()
{
}

func main() -> i32
{
  f();
  g();
}
```

### Pointer Types
| Syntax |
| ------ |
| *type  |

Note that the meaning is equivalent to that of a C pointer,<br/>
but the position of the * is opposite.

```rust
func main() -> i32
{
  let p: *i8 = "hello, world";

  let n: i32 = 4810;
  let p_n: *i32 = &n;

  let n = 4810;
  let p_n = &n;
}
```

### Array Types
| Syntax     |
| ---------- |
| type[size] |

```rust
func main() -> i32
{
  let a1: i32[4810];
  let a2 = {4, 8, 1, 0};
}
```

## Variables
### Definition
```rust
func main() -> i32
{
  let a: i32;
  let b = 48; // Type inference from the initializer.
}
```

### Mutable Variables
In this language, variables are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
func main() -> i32
{
  let i = 48; // Constant.
  i = 58; // Error!

  let mut j = 48; // Mutable.
  j = 58; // OK

  let mut k: i32; // Mutable.
  k = 4810; // OK
}
```

### Type Inference
Type inference can only be used for initializations with initializers.
```rust
func main() -> i32
{
  let n = 4810; // i32

  let f = 1 as bool; // bool

  let c = '💕'; // char

  let s = "hello, world"; // *i8
}
```

## Constant pointer
The constant pointer in this language is the same as 'const xxx* const' in c++.
```rust
func f(p: *i32)
{
  p = p; // Error!
  *p = 10; // Error!
}

func g(mut p: *i32)
{
  p = p; // OK!
  *p = 10; // OK!
}

func main() -> i32
{
  let n = 4810;
  f(&n);
  g(&n);
}
```

## Implicit Conversions
In the case of numeric values, operands with smaller bit widths are converted to the larger bit width side.
```rust
func main() -> i32
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
func twice(n: i32) -> i32
{
  return n * 2;
}
```

### Mutable Parameters
In this language, parameters are immutable by default.<br/>
To make them mutable, use the "mut" qualifier.
```rust
func f(n: i32) -> i32
{
  n = 123; // Error!
  return n;
}

func g(mut n: i32) -> i32
{
  n = 123; // OK
  return n;
}
```

### Linkage
```rust
func twice(n: i32) -> i32 // External linkage
{
  return n * 2;
}

func private thrice(n: i32) -> i32 // Internal linkage
{
  return n * 3;
}
```

## Identifier Naming Rules
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

## Integer Literal
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

## Character And String Encoding
The char type holds Unicode code points.
String literals are UTF8.

```rust
func main() -> i32
{
  let ch = 'あ'; // Unicode code point.

  let s = "良きに計らえ"; // UTF8.
}
```

## Array In Detail
### Definition
```rust
func main() -> i32
{
  let a1 = {'い', 'お'};

  let a2 = {'よ', 'は', 1, 0}; // Error!

  let a3: i32[4810];

  let a3: i32[]; // Error!
}
```

### Subscript
Subscript numbers begin with 0.
```rust
func f() -> i32
{
  return 0;
}

func main() -> i32
{
  let a = {48, 10, 4, 8, 1, 0};

  a[0];

  a[f() + 1];

  a[4810]; // Does not work correctly.

  for (let mut i = 0; i < 6; ++i)
    a[i];
}
```
Can also be used for pointers.
```rust
func front(p: *i32) -> i32
{
  return p[0];
}

func main() -> i32
{
  let a = {4, 8, 1, 0};
  front(&a[0]);
}
```

## Statements
### Expression Statements
```rust
func main() -> i32
{
  f();
}
```
```rust
func main() -> i32
{
  48 + 10;
}
```

### Compound Statement (Block)
```rust
func main() -> i32
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

### If-else Statement
```rust
func main() -> i32
{
  let cond: bool = true;

  if (cond) {
  }
  else
    ;
}
```

### While Statement
```rust
func main() -> i32
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

### For Statement
```rust
func main() -> i32
{
  // Repeat 10 times.
  for (let mut i = 0; i != 10; ++i) {
  }

  // infinite loop.
  for (;;)
    ;
}
```

### Break Statement
The break statement terminates execution of the nearest loop.
```rust
func main() -> i32
{
  for (;;) {
    break;
  }
}
```

### Continue Statement
```rust
func main() -> i32
{
  for (;;) {
    continue;
  }
}
```

### Return Statement
```rust
func main() -> i32
{
  return 48 + 10;
}
```
