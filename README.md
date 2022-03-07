<div align="center">
    <h1>The Miko Programming Language</h1>
</div>

## Syntax
### Example
```go
func twice(n)
{
    ret n * 2;
}

func main()
{
    let n = 58;
    ret twice(n) / 2;
}
```

```bash
$ echo $?
58
```
### Operators
#### Arithmetic operators
| Operator name  | Syntax |
| -------------- | ------ |
| Addition       | a + b  |
| Subtraction    | a - b  |
| Multiplication | a * b  |
| Division       | a / b  |
| Unary plus     | +a     |
| Unary minus    | -a     |

#### Comparison operators/relational operators
| Operator name            | Syntax  |
| ------------------------ | ------- |
| Equal to                 | a == b  |
| Not equal to             | a != b  |
| Greater than             | a > b   |
| Less than                | a < b   |
| Greater than or equal to | a >= b  |
| Less than or equal to    | a <= b |

#### Assignment operators
| Operator name     | Syntax |
| ----------------- | ------ |
| Direct assignment | a = b  |

#### Other operators
| Operator name | Syntax    |
| ------------- | --------- |
| Function call | a(a1, a2) |

This section on Operators is based on "Operators in C and C++" from wikipedia.<br/>
Thank you!

## References
- [X3 Documentation](http://ciere.com/cppnow15/x3_docs/): Documentation for Boost.Spirit.X3.

- [Using X3 Slides](http://ciere.com/cppnow15/x3_docs/): Slides for Boost.Spirit.X3.

- [LLVM Tutorial](https://llvm.org/docs/GettingStartedTutorials.html): Tutorials about using LLVM. Includes a tutorial about making a custom language with LLVM.

- [きつねさんでもわかるLLVM](https://tatsu-zine.com/books/llvm): あらゆる可能性を秘めたコンパイラ基盤として注目されているLLVM。本書はコンパイラを実際に作りながらLLVMのフロントエンドからバックエンドまでを幅広く解説した世界初(!?)のLLVM解説本です。

Thank you all!
