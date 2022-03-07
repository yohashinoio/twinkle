#!/bin/bash

# I used Ueyama rui's test of
# "https://www.sigbus.info/compilerbook"
# as a reference.

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
assert 25 "func g() { ret (10 + 20 - 5) * 2; } func f() { ret g() - 25; } func main() { ret f(); }"
assert 58 "extern clock(); func main() { clock(); ret 48 + 10; }"
assert 58 "func twice(n) { ret n * 2; } func main() { ret twice(29) + 1 - 1; }"
assert 58 "func add(a, b) { ret a + b; } func twice(n) { ret n * 2; } func main() { ret add(twice(add(4, 8) + 1 + 0), 32); }"

# hello, world
assert  58 "extern putchar(ch); func main() {
  putchar(104);putchar(101);putchar(108);putchar(108);putchar(111);putchar(44);
  putchar(32);putchar(119);putchar(111);putchar(114);putchar(108);putchar(100);
  putchar(10);
  ret 48 + 10;
}"

assert 58 "func main() { let a; ret a + 58; }"
assert 58 "func main() { let a; a = 48 + 10; ret a * 2 / 2; }"
assert 58 "func main() { let a = 4810; a = a / 2 / 5; ret a * 10 - 4810 + 58; }"

echo OK
