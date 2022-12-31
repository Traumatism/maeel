# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a concatenative [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built with ~1000 lines of Rust from scratch without any thirdparty library.

## Stack manipulation

| Name   | Description
| ---    | ---
| `pop`  | `a b c -- a b`
| `dup`  | `a b c -- a b c c`
| `rot`  | `a b c -- c b a`
| `swap` | `a b c -- a c b`

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
| `^`   | `a b -- a xor b`
| `!`   | `a -- ~a`
| `and` | `a b -- a&b`
| `or`  | `a b -- a\|b`

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
<condition> dup

if do
    <instruction>
end

!if do
    <instruction>
end

```


Example:

```
10 5 < dup

if do
    "10 < 5" println pop
end

!if do
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
1 2 3 4 5
rotate
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
```

Value can be a positive integer, a float, a string or a boolean.

## Negative integers

```
123 println
123! println
```

