#!/bin/bash

# Shell script for black box testing.

# These codes are licensed under Apache-2.0 License.
# See the LICENSE for details.
# Copyright (c) 2022 Hiramoto Ittou.

# This shell script is based on Ueyama rui's 9cc test.
# Thank you.

assert() {
  expected="$1"
  input="$2"

  # # compile and assemble
  # ../build/mikoc --input="$input"

  # # link
  # cc -static a.o
  # # Why is -static needed?
  # # https://www.sigbus.info/compilerbook#dynamic-linking

  # # execution
  # ./a.out

  # jit compile
  ../build/mikoc --jit --input="$input"

  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

# Four arithmetic operations
assert  0 "func main() -> i32 {ret 0;}"
assert 58 "func main() -> i32 {ret 58;}"

assert 58 "func main() -> i32 {ret 48 + 10;}"
assert 58 "func main() -> i32 {ret 48+10;}"
assert 38 "func main() -> i32 {ret 48 - 10;}"
assert 38 "func main() -> i32 {ret 48-10;}"
assert 32 "func main() -> i32 {ret 4 * 8;}"
assert  0 "func main() -> i32 {ret 1*0;}"
assert 48 "func main() -> i32 {ret 480 / 10;}"
assert 48 "func main() -> i32 {ret 480/10;}"
assert  8 "func main() -> i32 {ret 48 % 10;}"
assert  8 "func main() -> i32 {ret 48%10;}"

assert 10 "func main() -> i32 {ret 2 * 3 + 4;}"
assert 10 "func main() -> i32 {ret 2*3+4;}"
assert 14 "func main() -> i32 {ret 2 * (3 + 4);}"
assert 14 "func main() -> i32 {ret 2*(3+4);}"
assert  5 "func main() -> i32 {ret 12 / 3 + 1;}"
assert  5 "func main() -> i32 {ret 12/3+1;}"
assert  3 "func main() -> i32 {ret 12 / (3 + 1);}"
assert  3 "func main() -> i32 {ret 12/(3+1);}"

assert  9 "func main() -> i32 {ret 4 * 8 / 4 + 1 - 0;}"
assert  9 "func main() -> i32 {ret 4*8/4+1-0;}"

assert  3 "func main() -> i32 {ret 4 * 4 / (2 + 2) - 1;}"
assert  3 "func main() -> i32 {ret 4*4/(2+2)-1;}"
assert  3 "func main() -> i32 {ret 3 * 3 / ((2 + 2) - 1);}"
assert  3 "func main() -> i32 {ret 3*3/((2+2)-1);}"

# Unary operator
assert 38 "func main() -> i32 {ret 48 + -10;}"
assert 58 "func main() -> i32 {ret +48 + 10;}"
assert 58 "func main() -> i32 {ret 10 - -48;}"
assert 58 "func main() -> i32 {ret +10 - -48;}"
assert 48 "func main() -> i32 {ret +(-(48 * -(1 + 0)));}"

# Comparison operators and relational operators
assert  1 "func main() -> i32 {ret 1 + (48 == 10) as i32;}"
assert  1 "func main() -> i32 {ret 1+(48==10)as i32;}"
assert  2 "func main() -> i32 {ret 1 + (48 != 10) as i32;}"
assert  2 "func main() -> i32 {ret 1+(48!=10)as i32;}"
assert  1 "func main() -> i32 {ret 1 + (48 < 10) as i32;}"
assert  1 "func main() -> i32 {ret 1+(48<10)as i32;}"
assert  2 "func main() -> i32 {ret 1 + (48 > 10) as i32;}"
assert  2 "func main() -> i32 {ret 1+(48>10)as i32;}"
assert  2 "func main() -> i32 {ret 1 + (48 <= 48) as i32;}"
assert  2 "func main() -> i32 {ret 1+(48<=48)as i32;}"
assert  2 "func main() -> i32 {ret 1 + (10 >= 10) as i32;}"
assert  2 "func main() -> i32 {ret 1+(10>=10)as i32;}"

# Expression statement
assert 48 "func main() -> i32 {1+(48>10)as i32; ret +(-(48 * -(1 + 0)));}"

# Function
assert 25 "func g() -> i32 {
  ret (10 + 20 - 5) * 2;
}
func f() -> i32 {
  ret g() - 25;
}
func main() -> i32 {
  ret f();
}"

assert 58 "extern clock() -> i32;
func main() -> i32 {
  clock(); ret 48 + 10;
}"

assert 58 "func twice(n: i32) -> i32 {
  ret n * 2;
}
func main() -> i32 {
  ret twice(29) + 1 - 1;
}"

assert 58 "func add(a: i32, b: i32) -> i32 {
  ret a + b;
}
func twice(n: i32) -> i32 {
  ret n * 2;
}
func main() -> i32 {
  ret add(twice(add(4, 8) + 1 + 0), 32);
}"

# hello, world
assert  58 "extern putchar(ch: i32) -> i32;
func main() -> i32 {
  putchar(104);putchar(101);putchar(108);putchar(108);putchar(111);putchar(44);
  putchar(32);putchar(119);putchar(111);putchar(114);putchar(108);putchar(100);
  putchar(10);
  ret 48 + 10;
}"

# Variable
assert 58 "func main() -> i32 {
  let a: i32 = 0;
  ret a + 58;
}"
assert 58 "func main() -> i32 {
  let mut a: i32;
  a = 48 + 10;
  ret a * 2 / 2;
}"
assert 58 "func main() -> i32 {
  let mut a: i32 = 4810;
  a = a / 2 / 5;
  ret a * 10 - 4810 + 58;
}"
assert 58 "func main() -> i32 {
  let a: i32 = 24 * 2;
  let b: i32 = 5 * 2;
  ret a + b;
}"

# If statement
assert 48 "func main() -> i32 {
  let n: i32 = 4;
  if (n == 4)
    ret 48;
  else
    ret 10;
}"
assert 10 "func main() -> i32 {
  let n: i32 = 4;
  if (n != 4) {
    ret 48;
  }
  else if (n == 123) {
    ret 1;
  }
  else
    ret 10;
}"
assert 58 "func main() -> i32 {
  let a: i32 = 4810;
  if (a == 4810) {
    let b: i32 = 110;
    if (b == 4810)
      ret 1;
    else if (b == 110) {
      let mut i: i32;
      for (i = 0; i < 58; i = i + 1) ;
      ret i;
    }
    else
      ret 2;
  }
  ret 3;
}"
assert 58 "func main() -> i32 {
  let n: i32 = 4810;
  if (n != 4810)
    ret 0;
  else if (n == 4810) {
    if (0)
      ret 1;
    ret 58;
  }
  else
    ret 123;
}"
assert 58 "func main() -> i32 {
  let n: i32 = 58;
  if (1) {}
  if (1) ;
  ret n;
}"

# Fibonacci numbers
assert 58 "func fib(n: i32) -> i32 {
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);
}
func main() -> i32
{
  if (fib(10) == 55)
    ret 58;
}"

# For statement
assert 58 "func main() -> i32 {
  let mut i: i32;
  let mut n: i32 = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
  }
  let mut j: i32;
  for (j = 0; j < 48; j = j + 1)
    n = n + 1;
  ret n;
}"
assert 58 "func main() -> i32 {
  let i: i32 = 58;

  for (; i < 10; ) ;

  ret i;
}"
assert 58 "func main() -> i32 {
  let mut i: i32 = 0;
  for (;; i = i + 1) {
    if (i == 58)
      ret i;
  }
  ret 123;
}"
assert 110 "func main() -> i32 {
  let mut i: i32;
  let mut j: i32;
  let mut n: i32 = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
    for (j = 0; j < 10; j = j + 1)
      n = n + 1;
  }
  ret n;
}"
assert 58 "func main() -> i32 {
  let mut i: i32;
  for (i = 0; i < 4810; i = i + 1) {
    let n: i32 = i;
    if (n == 58)
      ret n;
  }
}"

# Comments
assert 58 "func main() -> i32 {
  // return abc;

  /*
  let 123;
  */

  ret 58;
}"

# Linkage
assert 58 "func private f() -> i32 {
  ret 58;
}
func main() -> i32 {
  ret f();
}"

# Return type
assert 58 "func f() -> i32 {
  ret 58;
}
func main() -> i32 {
  ret f();
}"

# Variable type
assert 58 "func main() -> i32 {
  let a: i8;
  let b: u8;

  let c: i16;
  let d: u16;

  let e: i32;
  let f: u32;

  let g: i64;
  let h: u64;

  let k: bool;

  ret 58;
}"

# Parameter type
assert 58 "func equal(a: i32, b: i32) -> bool {
  ret a == b;
}
func main() -> i32 {
  let a: i32 = 48;
  let b: i32 = 10;
  if (equal(a, b))
    ret 0;
  else
    ret 58;
}"

assert 58 "func f(mut a: i32) -> i32 {
  ret a = 58;
}
func main() -> i32 {
  let n: i32 = 4810;
  ret f(n);
}"

# Integer cast
assert 1 "func main() -> i32 {
  let n: bool = 1 as bool;
  ret n as i32;
}"

assert 58 "func main() -> i64 {
  let a: i8 = -58 as i8;
  let b: u8 = 58 as u8;

  let c: i16 = -4810 as i16;
  let d: u16 = 4810 as u16;

  let e: i32 = -48104810 as i32;
  let f: u32 = 48104810 as u32;

  let g: i64 = -48104810 as i64;
  let h: u64 = 4148104810 as u64;

  let k: bool = 1 as bool;
  let l: bool = 0 as bool;

  ret 58 as i64;
}"

# Void type
assert 58 "extern putchar(ch: i32) -> i32;
func f() -> void {
  putchar(52);
  putchar(56);
  putchar(49);
  putchar(48);
  putchar(10);
}
func main() -> i32 {
  f();
  ret 58;
}"

assert 58 "extern putchar(ch: i32) -> i32;
func f() -> void {
  putchar(53);
  putchar(56);
  putchar(10);
  ret;
  putchar(10);
  ret;
  putchar(10);
  ret;
}
func main() -> i32 {
  f();
  ret 58;
}"

# String literal
assert 58 'func main() -> i32 {
  "";
  " ";
  "a";
  "abc";
  ret 58;
}'

assert 58 'func yoha() -> i8 {
  "hello, yoha";
  ret 48 as i8;
}
func shino(yoha: i8, io: i8) -> i32 {
  "hello, yoha shino io";
  ret (yoha + io) as i32;
}
func io() -> i8 {
  "hello, io";
  ret 10 as i8;
}
func main() -> i32 {
  ret shino(yoha(), io());
}'

# Escape sequence
assert 58 'func main() -> i32 {
  "hello\aworld";"hello\bworld";
  "hello\fworld";"hello\nworld";
  "hello\rworld";"hello\tworld";
  "hello\vworld";"hello\0world";
  "hello\\world";"hello\"world";
  ret 58;
}'

# Pointer type
assert 58 "func f() -> i32 {
  let p1: *i16;
  let p2: *u16;
  let p3: *i64;
  let p4: *u64;
  ret 58;
}
func main() -> i32 {
  let s1: *i8;
  let mut s2: *u8;
  let p1: *i32;
  let mut p2: *u32;
  ret f();
}"

assert 58 'extern puts(s: *i8) -> i32;
func main() -> i32 {
  let s1: *i8 = "hello, yoha, shino, io";
  puts(s1);
  let mut s2: *i8 = "hello\0, world";
  puts(s2);
  ret 58;
}'

# Addition assignment operator
assert 58 "func main() -> i32 {
  let mut shino: i32 = 0;
  shino += 48;
  shino += 10;
  ret shino;
}"

assert 116 "func main() -> i32 {
  let mut shino: i32 = 0;
  shino += 48;
  ret (shino += 10) * 2;
}"

# Subtraction assignment operator
assert 58 "func main() -> i32 {
  let mut shino: i32 = 68;
  shino -= 10;
  ret shino;
}"

assert 116 "func main() -> i32 {
  let mut shino: i32 = 68;
  ret (shino -= 10) * 2;
}"

# Multiplication assignment operator
assert 58 "func main() -> i32 {
  let mut shino: i32 = 29;
  shino *= 2;
  ret shino;
}"

assert 116 "func main() -> i32 {
  let mut shino: i32 = 29;
  ret (shino *= 2) * 2;
}"

# Division assignment operator
assert 58 "func main() -> i32 {
  let mut shino: i32 = 116;
  shino /= 2;
  ret shino;
}"

assert 116 "func main() -> i32 {
  let mut shino: i32 = 116;
  ret (shino /= 2) * 2;
}"

# Modulo assignment operator
assert 1 "func main() -> i32 {
  let mut shino: i32 = 4810;
  shino %= 3;
  ret 1;
}"

assert 58 "func main() -> i32 {
  let mut shino: i32 = 4810;
  ret (shino %= 58) + 4;
}"

# Boolean literals
assert 0 "func main() -> i32 {
  let f1: bool = true;
  let f2: bool = false;
  let mut f3: bool;
  f3 = true;
  f3 = false;
  ret f3 as i32;
}"

assert 2 "func main() -> i32 {
  let f: bool = true;
  ret (f + false + true - false) as i32;
}"

echo OK
