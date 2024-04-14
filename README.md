# maeel

### a minimalistic stack based programming language.

The interpreter is made of ~ 600 lines of code :3 Indeed a lot of maeel features are implemented in itself (stack functions, while loop, for loop, arrays, functional programming utilities...) (c.f. [maeel.maeel](maeel.maeel))

## Project advancement

- Lexer: `100%`
- Interpreter: `90%`
- Compiler: `10%`

## Compile

`$ make test` (c.f. [tests.maeel](./stdlib/tests.maeel))

`$ make bench`

`$ make`

## Run program

`$ ./maeel program.maeel`

## Hello, world!

```
"Hello, world" puts
```

## Variables

```
<anything on the stack> ~ <variable name>
```

```
"hello" ~ hello
```

## Functions

```
fun name x y z (x y + z *)
```

maeel also supports inline functions:

```
fun inline name (+ *)
```

## Conditions

```
<boolean on the stack> ? (<executed if boolean is 1>)
```

```
<boolean on the stack> (<executed if true>) (<executed if false>) ifelse
```
