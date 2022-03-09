#!/bin/bash

# These codes are licensed under Apache-2.0 License.
# See the LICENSE for details.
# Copyright (c) 2021 Hiramoto Ittou.

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

assert  0 "func main() {ret 0;}"
assert 42 "func main() {ret 42;}"

assert 58 "func main() {ret 48 + 10;}"
assert 58 "func main() {ret 48+10;}"
assert 38 "func main() {ret 48 - 10;}"
assert 38 "func main() {ret 48-10;}"
assert 32 "func main() {ret 4 * 8 * 1 + 0;}"
assert 32 "func main() {ret 4*8*1+0;}"
assert 10 "func main() {ret 4810 / 481;}"
assert 10 "func main() {ret 4810/481;}"

assert 36 "func main() {ret 4 * (8 + 1) + 0;}"
assert 36 "func main() {ret 4*(8+1)+0;}"

assert 38 "func main() {ret 48 + -10;}"
assert 58 "func main() {ret +48 + 10;}"
assert 58 "func main() {ret 10 - -48;}"
assert 58 "func main() {ret +10 - -48;}"
assert 48 "func main() {ret +(-(48 * -(1 + 0)));}"

assert  1 "func main() {ret 1 + (48 == 10);}"
assert  1 "func main() {ret 1+(48==10);}"
assert  2 "func main() {ret 1 + (48 != 10);}"
assert  2 "func main() {ret 1+(48!=10);}"
assert  1 "func main() {ret 1 + (48 < 10);}"
assert  1 "func main() {ret 1+(48<10);}"
assert  2 "func main() {ret 1 + (48 > 10);}"
assert  2 "func main() {ret 1+(48>10);}"
assert  2 "func main() {ret 1 + (48 <= 48);}"
assert  2 "func main() {ret 1+(48<=48);}"
assert  2 "func main() {ret 1 + (10 >= 10);}"
assert  2 "func main() {ret 1+(10>=10);}"

assert 48 "func main() { 1+(48>10); ret +(-(48 * -(1 + 0))); }"

# Function
assert 25 "func g() {
  ret (10 + 20 - 5) * 2;
}
func f() {
  ret g() - 25;
}
func main() {
  ret f();
}"

assert 58 "extern clock();
func main() {
  clock(); ret 48 + 10;
}"

assert 58 "func twice(n) {
  ret n * 2;
}
func main() {
  ret twice(29) + 1 - 1;
}"

assert 58 "func add(a, b) {
  ret a + b;
}
func twice(n) {
  ret n * 2;
}
func main() {
  ret add(twice(add(4, 8) + 1 + 0), 32);
}"

# hello, world
assert  58 "extern putchar(ch); func main() {
  putchar(104);putchar(101);putchar(108);putchar(108);putchar(111);putchar(44);
  putchar(32);putchar(119);putchar(111);putchar(114);putchar(108);putchar(100);
  putchar(10);
  ret 48 + 10;
}"

# Variable
assert 58 "func main() {
  let a = 0;
  ret a + 58;
}"
assert 58 "func main() {
  let a; a = 48 + 10;
  ret a * 2 / 2;
}"
assert 58 "func main() {
  let a = 4810;
  a = a / 2 / 5;
  ret a * 10 - 4810 + 58;
}"
assert 58 "func main() {
  let a = 24 * 2;
  let b = 5 * 2; ret a + b;
}"

# If statement
assert 48 "func main() {
  let n = 4;
  if (n == 4)
    ret 48;
  else
    ret 10;
}"
assert 10 "func main() {
  let n = 4;
  if (n != 4) {
    ret 48;
  }
  else if (n == 123) {
    ret 1;
  }
  else
    ret 10;
}"
assert 58 "func main() {
  let a = 4810;
  if (a == 4810) {
    let b = 110;
    if (b == 4810)
      ret 1;
    else if (b == 110) {
      let i;
      for (i = 0; i < 58; i = i + 1) ;
      ret i;
    }
    else
      ret 2;
  }
  ret 3;
}"
assert 58 "func main() {
  let n = 4810;
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

# Fibonacci numbers
assert 58 "func fib(n) {
  if (n < 3)
    ret 1;
  else
    ret fib(n - 1) + fib(n - 2);
  ret 0;
}
func main()
{
  if (fib(10) == 55)
    ret 58;
}"

# For statement
assert 58 "func main() {
  let i; let n = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
  }
  let j;
  for (j = 0; j < 48; j = j + 1)
    n = n + 1;
  ret n;
}"
assert 58 "func main() {
  let i = 58;

  for (; i < 10; ) ;

  ret i;
}"
assert 58 "func main() {
  let i = 0;
  for (;; i = i + 1) {
    if (i == 58)
      ret i;
  }
  ret 123;
}"
assert 110 "func main() {
  let i; let j; let n = 0;
  for (i = 0; i < 10; i = i + 1) {
    n = n + 1;
    for (j = 0; j < 10; j = j + 1)
      n = n + 1;
  }
  ret n;
}"


echo OK
