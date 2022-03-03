#!/bin/bash

# I used Ueyama rui's test of
# "https://www.sigbus.info/compilerbook"
# as a reference.

assert() {
  expected="$1"
  input="$2"

  ../build/mikoc "$input"
  cc a.o
  ./a.out
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0  "fn main() {0}"
assert 42 "fn main() {42}"
assert 58 "fn main() {48 + 10}"
assert 58 "fn main() {48+10}"
assert 38 "fn main() {48 - 10}"
assert 38 "fn main() {48-10}"
assert 32 "fn main() {4 * 8 * 1 + 0}"
assert 32 "fn main() {4*8*1+0}"
assert 10 "fn main() {4810 / 481}"
assert 10 "fn main() {4810/481}"
assert 36 "fn main() {4 * (8 + 1) + 0}"
assert 36 "fn main() {4*(8+1)+0}"
assert 38 "fn main() {48 + -10}"
assert 58 "fn main() {+48 + 10}"
assert 58 "fn main() {10 - -48}"
assert 58 "fn main() {+10 - -48}"
assert 48 "fn main() {+(-(48 * -(1 + 0)))}"
# The reason why "1 +" is written at the beginning is
# to prevent the return value from being a boolean(i1 in llvm ir) type.
assert  1 "fn main() {1 + (48 == 10)}"
assert  1 "fn main() {1+(48==10)}"
assert  2 "fn main() {1 + (48 != 10)}"
assert  2 "fn main() {1+(48!=10)}"
assert  1 "fn main() {1 + (48 < 10)}"
assert  1 "fn main() {1+(48<10)}"
assert  2 "fn main() {1 + (48 > 10)}"
assert  2 "fn main() {1+(48>10)}"
assert  2 "fn main() {1 + (48 <= 48)}"
assert  2 "fn main() {1+(48<=48)}"
assert  2 "fn main() {1 + (10 >= 10)}"
assert  2 "fn main() {1+(10>=10)}"

echo OK
