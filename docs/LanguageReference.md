## Table of Contents
- [Table of Contents](#table-of-contents)
- [Comments](#comments)
  - [Single line comment](#single-line-comment)
  - [Multi line comment](#multi-line-comment)
- [Operators](#operators)
  - [Arithmetic operators](#arithmetic-operators)
  - [Comparison operators / Relational operators](#comparison-operators--relational-operators)
  - [Assignment operators (Statement)](#assignment-operators-statement)
  - [Pointer operators](#pointer-operators)
  - [Other operators](#other-operators)
- [Fundamental (Built-in) Types](#fundamental-built-in-types)
  - [Integer types](#integer-types)
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

### Array types
| Syntax     |
| ---------- |
| type[size] |

```rust
let a1: i32[4810];
let a2 = {4, 8, 1, 0};
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
  ++i;
}

while (/* Required */) // Error!
  ;
```

### For statement
```rust
// Repeat 10 times.
for (let mut i = 0; i != 10; ++i) {
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
