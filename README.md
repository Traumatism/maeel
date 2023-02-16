# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a concatenative [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built with ~800 lines of Rust from scratch without any thirdparty library.

## Stack manipulation

| Name   | Description
| ---    | ---
| `pop`  | `a -- a`
| `dup`  | `a -- a a`
| `swap` | `a b -- b a`
| `over` | `a b -- a b a`

## Arithmetic operations

| Name | Description
| ---  | ---
| `+`  | `a b -- a+b`
| `-`  | `a b -- a-b`
| `*`  | `a b -- a*b`
| `/`  | `a b -- a/b`
| `%`  | `a b -- a%b`

## Comparison

| Name  | Description
| ---   | ---
| `=`   | `a b -- a==b`
| `<`   | `a b -- a<b`
| `>`   | `a b -- a>b`
| `!`   | `a -- not a`
| `*`   | `a b -- a and b`
| `+`   | `a b -- a or b`

## Procedures

```
proc <name> do
    <instructions>
end
```

## If conditions

```
<condition> if do
    <instructions>
end
```

Example:

```
10 5 > if do
    "10 > 5" println pop
end
```

## If-else

```
<condition> let comp pop

comp if do
    <instruction>
end

comp! if do
    <instruction>
end

```


Example:

```
10 5 < let comp pop

comp if do
    "10 < 5" println pop
end

comp! if do
    "10 >= 5" println pop
end
```

## For loops

```
<array> for do
    <instruction>
end
```

Example:

```
5 4 3 2 1
5 take for do
    "Iteration #" print pop println pop
end
```

## While loops

```
<condition> while do
    <instruction>
    <condition>
end
```

Example:

```
let n 0

n 10 < while do
    n 1 + let n dup
    "Iteration #" print pop println pop
    n 10 <
end
```

## Variables

```
let <name> <value>

<value> let <name> pop
<value> let <name> dup
<value> let <name> over
```

Value can be a positive integer, a float, a string or a boolean.

## Negative integers

```
123 println
123! println
```

