# Overview

Maeel is an interpreted programming language built on top of Rust.

## Concatenative and stack oriented

Maeel use a concatenative syntax to understand what programmer want.
Instructions are read from left to right and should be written using reverse
polish notation. It also uses a stack.

Traditional programming language: `(5*2)+4`

In, maeel, it would written: `5 2 * 4 +`

- push 5
- push 2
- multiply (=> push 5*2)
- push 4
- add (=> push 10+4)

## Variables?

Unfortunatly, you can't do everything using only basic stack functions. That why maeel also has a variable system.
Assignment are fully dynamic:

```
4 → four
44 → four
```

- push 4
- assign the STV (stack top value) to four
- push 44
- reassign the STV to four

There is also way to declare code block specific (=private) variables: simply add a _ at the begin of the variable name.

## Functionnal!

Maeel uses only functions with no side effects: no structures, no classes.
They are defined with the lambda letter followed by parameters, and code block.

```
λ foo [bar] (
    bar print
)

"Hello, world\n" foo
```

Since stack is shared between functions, you can also declare non-args functions!

```
λ foo [] (print)

"Hello, world\n" foo
```

## Looooops

Maeel for/while loops implementation is pretty bad.