# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built in a single Rust file from scratch without any thirdparty library.


## Examples

- [Hello, world!](./exemples/hello_world.maeel)
- [output](./exemples/print.maeel)
- [arrays](./exemples/arrays.maeel)
- [login](./exemples/logic.maeel)
- [conditions](./exemples/conditions.maeel)



## Add implementation

- string + string $\rightarrow$ concatenate strings (string)
- int + int $\rightarrow$ sum (int)
- float + float $\rightarrow$ sum (float)
- int + float $\rightarrow$ sum (float)
- float + int $\rightarrow$ sum (float)
- other $\rightarrow$ panic

## Mul implementation

- int * int $\rightarrow$ product (int)
- float * float $\rightarrow$ product (float)
- int * float $\rightarrow$ product (float)
- float * int $\rightarrow$ product (float)
- other $\rightarrow$ panic

## Sub implementation

- m - n with m, n ints $\rightarrow$ m - n (int)
- x - y with x, y floats $\rightarrow$ x - y (float)
- n - x with n int and x float $\rightarrow$ n - x (float)
- x - n with x float and n int $\rightarrow$ x - n (float)
- other $\rightarrow$ panic

## Bitwise xor implementation

- bool xor bool $\rightarrow$ bool
- other $\rightarrow$ panic

## Bitwise or implementation

- bool or bool $\rightarrow$ bool
- other $\rightarrow$ panic

## Bitwise and implementation

- bool and bool $\rightarrow$ bool
- other $\rightarrow$ panic

## Not implementation

- not bool $\rightarrow$ bool
- not int $\rightarrow$ int * -1 (int)
- not float $\rightarrow$ float * -1 (float)