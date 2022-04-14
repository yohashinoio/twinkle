<div align="center">
  <h1>The Maple Programming Language</h1>

  [Getting Started](docs/GettingStarted.md) |
  [Language Reference](docs/LanguageReference.md) |
  [Examples](examples)
</div>

## Key Features of Maple
- Variables are immutable by default.
- Support for both AOT and JIT compilation.
- LLVM backend.

## Installing
### Dependency List
- LLVM (Developed in 14.0.0)
- Boost (Developed in 1.71.0)
- CMake
- Make (Of course, ninja will work)
- C++20 compiler (Developed with GCC 8.4.0 and clang 14.0.0)

### Install dependencies (Ubuntu)
Here is how to install them in ubuntu.
```bash
$ sudo apt update -y
```
Install GCC Make CMake Boost.
```bash
$ sudo apt install -y build-essential cmake libboost-all-dev
```
Install LLVM. (https://apt.llvm.org/)
```bash
$ sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
```
Please make sure you can use the llvm-config command.<br/>
If it is llvm-config-xxx, please use symbolic links, etc.<br/>
Here is an example if /usr/bin/llvm-config-xxx.
```bash
$ sudo ln -s /usr/bin/llvm-config-xxx /usr/local/bin/llvm-config
```

### Clone
```bash
$ git clone https://github.com/GothicLoli/maple.git
$ cd maple
```

### Build
Create a build directory.
```bash
$ mkdir build && cd $_
```
Next, build. (It may take a few minutes)
```bash
$ cmake .. -DCMAKE_BUILD_TYPE=Release
$ sudo make install
```
If you want to specify where to install.
```bash
$ cmake .. -DCMAKE_INSTALL_PREFIX="path/to/install" -DCMAKE_BUILD_TYPE=Release
$ make install
```

## References
- [X3 Documentation](http://ciere.com/cppnow15/x3_docs/): Documentation for Boost.Spirit.X3.
- [Using X3 Slides](http://ciere.com/cppnow15/x3_docs/): Slides for Boost.Spirit.X3.
- [LLVM Tutorial](https://llvm.org/docs/GettingStartedTutorials.html): Tutorials about using LLVM. Includes a tutorial about making a custom language with LLVM.
- [きつねさんでもわかるLLVM](https://tatsu-zine.com/books/llvm): あらゆる可能性を秘めたコンパイラ基盤として注目されているLLVM。本書はコンパイラを実際に作りながらLLVMのフロントエンドからバックエンドまでを幅広く解説した世界初(!?)のLLVM解説本です。

## License
This project is available under the Apache-2.0 license.<br/>
See LICENSE for the full content of the licenses.
