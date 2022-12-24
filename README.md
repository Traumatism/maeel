# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built in a single Rust file from scratch without any thirdparty library.


## Examples

- [Hello, world!](./examples/hello_world.maeel)
- [output](./examples/print.maeel)
- [arrays](./examples/arrays.maeel)
- [login](./examples/logic.maeel)
- [conditions](./examples/conditions.maeel)

### Implemented in itself

- [while-loops](./examples/while.maeel)
- [range](./examples/range.maeel)
- [fibonacci](./examples/fibonacci.maeel)

## Example of implementing fibonacci in maeel

```
proc fib
    let n pop
    n dup dup 1= swap 0= |! if
        dup 1- fib swap 2- fib +
        return
    end
end_proc

proc fib_list_inner
    a 20= if
        return
    end

    a fib print pop " " print pop
    a 1+ let a pop fib_list_inner
end_proc

let a 0
fib_list_inner
```
