## Table of Contents
- [Table of Contents](#table-of-contents)
- [Notes](#notes)
- [Hello World](#hello-world)
- [AOT Compile](#aot-compile)
- [JIT Compile](#jit-compile)

## Hello World
```rust
// hello.txt
declare func printf(fmt: *i8, ...) -> i32;

func main() -> i32
{
  printf("hello, world\n");
}
```
Input files must be encoded in UTF8.

## AOT Compile
This is the standard compilation method for C, C++, etc.<br/>

If multiple files are passed, they are not linked and each is compiled to the target.<br/>
The target defaults to an object file, which can be changed using the emit option.<br/>
See help for more details.<br/>

Currently, this compiler does not support linking, so you must use another program to link.<br/>
The following example uses cc to link with libc.
```bash
$ maplec hello.txt
$ cc hello.o
$ ./a.out
hello, world
```

## JIT Compile
If multiple files are passed, they are linked and executed.<br/>
Therefore, if the same symbol is defined, an error will occur.
```bash
$ maplec --JIT hello.txt
hello, world
```
