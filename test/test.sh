#!/bin/bash

# These codes are licensed under Apache-2.0 License.
# See the LICENSE for details.
# Copyright (c) 2022 Hiramoto Ittou.

# This shell script is based on Ueyama rui's 9cc test.
# Thank you.


assert() {
  expected="$1"
  input="$2"

  ../build/mikoc --input="$input"
  cc input.o
  ./a.out
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
  var a: i32 = 0;
  ret a + 58;
}"
assert 58 "func main() -> i32 {
  var mut a: i32;
  a = 48 + 10;
  ret a * 2 / 2;
}"
assert 58 "func main() -> i32 {
  var mut a: i32 = 4810;
  a = a / 2 / 5;
  ret a * 10 - 4810 + 58;
}"
assert 58 "func main() -> i32 {
  var a: i32 = 24 * 2;
  var b: i32 = 5 * 2;
  ret a + b;
}"

# If statement
assert 48 "func main() -> i32 {
  var n: i32 = 4;
  if (n == 4)
    ret 48;
  else
    ret 10;
}"
assert 10 "func main() -> i32 {
  var n: i32 = 4;
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
  var a: i32 = 4810;
  if (a == 4810) {
    var b: i32 = 110;
    if (b == 4810)
      ret 1;
    else if (b == 110) {
      var mut i: i32;
      for (i = 0; i < 58; i = i + 1) ;
      ret i;
    }
    else
      ret 2;
  }
  ret 3;
}"
assert 58 "func main() -> i32 {
  var n: i32 = 4810;
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
  var n: i32 = 58;
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
  ret 0;
}
func main() -> i32
{
  if (fib(10) == 55)
    ret 58;
}"

# For statement
assert 58 "func main() -> i32 {
  var mut i: i32;
  var mut n: i32 = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
  }
  var mut j: i32;
  for (j = 0; j < 48; j = j + 1)
    n = n + 1;
  ret n;
}"
assert 58 "func main() -> i32 {
  var i: i32 = 58;

  for (; i < 10; ) ;

  ret i;
}"
assert 58 "func main() -> i32 {
  var mut i: i32 = 0;
  for (;; i = i + 1) {
    if (i == 58)
      ret i;
  }
  ret 123;
}"
assert 110 "func main() -> i32 {
  var mut i: i32;
  var mut j: i32;
  var mut n: i32 = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
    for (j = 0; j < 10; j = j + 1)
      n = n + 1;
  }
  ret n;
}"
assert 58 "func main() -> i32 {
  var mut i: i32;
  for (i = 0; i < 4810; i = i + 1) {
    var n: i32 = i;
    if (n == 58)
      ret n;
  }
}"

# Comments
assert 58 "func main() -> i32 {
  // return abc;

  /*
  var 123;
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
  var a: i8;
  var b: u8;

  var c: i16;
  var d: u16;

  var e: i32;
  var f: u32;

  var g: i64;
  var h: u64;

  var k: bool;

  ret 58;
}"

# Parameter type
assert 58 "func equal(a: i32, b: i32) -> bool {
  ret a == b;
}
func main() -> i32 {
  var a: i32 = 48;
  var b: i32 = 10;
  if (equal(a, b))
    ret 0;
  else
    ret 58;
}"

assert 58 "func f(mut a: i32) -> i32 {
  ret a = 58;
}
func main() -> i32 {
  var n: i32 = 4810;
  ret f(n);
}"

# Integer cast
assert 1 "func main() -> i32 {
  var n: bool = 1 as bool;
  ret n as i32;
}"

assert 58 "func main() -> i64 {
  var a: i8 = -58 as i8;
  var b: u8 = 58 as u8;

  var c: i16 = -4810 as i16;
  var d: u16 = 4810 as u16;

  var e: i32 = -48104810 as i32;
  var f: u32 = 48104810 as u32;

  var g: i64 = -48104810 as i64;
  var h: u64 = 4148104810 as u64;

  var k: bool = 1 as bool;
  var l: bool = 0 as bool;

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

# Pointer type (parsing)
assert 58 "func f() -> i32 {
  var p1: *i16;
  var p2: *u16;
  var p3: *i64;
  var p4: *u64;
  ret 58;
}
func main() -> i32 {
  var s1: *i8;
  var mut s2: *u8;
  var p1: *i32;
  var mut p2: *u32;
  ret f();
}"

echo OK
