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
| `+`  | `a b c -- a b+c`
| `-`  | `a b c -- a b-c`
| `*`  | `a b c -- a b*c`
| `/`  | `a b c -- a b/c`
| `%`  | `a b c -- a b%c`

## Comparison

| Name  | Description
| ---   | ---
| `=`   | `a b -- a==b`
| `<`   | `a b -- a<b`
| `>`   | `a b -- a>b`
| `^`   | `a b -- a xor b`
| `!`   | `a -- ~a`
| `and` | `a b -- a&b`
| `or`  | `a b -- a|b`

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

## For loops

```
<array> for do
    <instruction>
end
```

## While loops

```
<condition> while do
    <instruction>
    <condition>
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

