#!/bin/bash

# I used Ueyama rui's test of
# "https://www.sigbus.info/compilerbook"
# as a reference.

assert() {
  expected="$1"
  input="$2"

  ../build/mikoc "$input"
  lli a.bc
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 58 "48 + 10"
assert 58 "48+10"
assert 38 "48 - 10"
assert 38 "48-10"
assert 32 "4 * 8 * 1 + 0"
assert 32 "4*8*1+0"
assert 10 "4810 / 481"
assert 10 "4810/481"
assert 36 "4 * (8 + 1) + 0"
assert 36 "4*(8+1)+0"
assert 38 "48 + -10"
assert 58 "+48 + 10"
assert 58 "10 - -48"
assert 48 "+(-(48 * -(1 + 0)))"
# The reason why "1 +" is written at the beginning is
# to prevent the return value from being a boolean(i1 in llvm ir) type.
assert  1 "1 + (48 == 10)"
assert  2 "1 + (48 != 10)"
assert  1 "1 + (48 < 10)"
assert  2 "1 + (48 > 10)"
assert  2 "1 + (48 <= 48)"
assert  2 "1 + (10 >= 10)"

echo OK
