## Introduction

maeel is a functionnal stack-oriented programming language that uses concatenation to parse instructions.


### Example

```
"std" include

2 3 + putsln
```

> First, we push the string `"std"` and then applies the `include` function. The include function takes the stack top element (must be a string, in this case), include the tokens and re-process it (=gather the functions...). Note that now the string `"std"` is not on the stack anymore, has it have been pop by the `include` function.

> Then, we push integers `2` and `3` on the stack and apply the `+` binary operator. The `+` operator pops stack 2-topmosts values and push their sum.

> Finally, we use `putsln` (from `std` library) that pops the stack topmost value to stdout (To not be confused with `println` that just print the element without popping it).