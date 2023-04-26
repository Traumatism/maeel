# The maeel programming language

Maeel is a { concatenative, esotheric, stack-based, interpreted } programming language.

Important concepts:
    - https://en.wikipedia.org/wiki/Stack_machine
    - https://en.wikipedia.org/wiki/Reverse_Polish_notation
    - https://en.wikipedia.org/wiki/Stack-oriented_programming

# Patterns

## Variable definition

`→ name` will set the "name" alias to the value on the top of the stack.

Example:

```
3.14 → pi

pi print
```

## Function definition

`λ name (code)` will set the "name" alias to the next code block.

Example:

```
λ square (
    → arg1
    arg1 arg1 *
)

2 square print
```

## If

`⇒ (code)` will execute "code" if, and only if the stack top value is `true`

Example:

`a b = ⇒ ("a and b are equal!" print)`

## While loop

`ω (code)` will loop executing "code" while there is a `true` on the stack top. Note: the "code" needs to output a boolean at its end to define if it will continue looping or not.

Example:

```
1 → a

a 100 < ω (
    a print
    a 1 + → a

    a 100 <
)
```

## For loop

`Σ (code)` will push(value) for all value element of stack top (stack top must be an array) and then execute the next code block

# Operators

## Stack functions

| Symbol | Definition         |
|---     |---                 |
| `ρ`    | `a b c -- a b`     |
| `σ`    | `a b c -- a c b`   |
| `ψ`    | `a b c -- c a b`   |
| `δ`    | `a b c -- a b c c` |
| `θ`    | `a b c -- a b c b` |

## Arithmetics

| Symbol | Definition       | Commutative |
|---     |---               |---          |
| `+`    | `x y -- (x + y)` | yes         |
| `-`    | `x y -- (x - y)` | no          |
| `*`    | `x y -- (x * y)` | yes         |
| `/`    | `x y -- (x / y)` | no          |
| `%`    | `x y -- (x % y)` | no          |

## Logic

| Symbol | Definition         | Commutative |
|---     |---                 |---          |
| `∧`    | `p q -- (p and q)` | yes         |
| `∨`    | `p q -- (p or q)`  | yes         |
| `⊕`    | `p q -- (p xor q)` | yes         |

## Comparison

| Symbol | Definition          | Commutative |
|---     |---                  |---          |
| `=`    | `a b -- (a = b)`    | yes         |
| `≠`    | `a b -- (¬(a = b))` | yes         |
| `>`    | `a b -- (a > b)`    | no          |
| `<`    | `a b -- (a < b)`    | no          |
| `⩽`    | `a b -- (a ⩽ b)`    | no          |
| `⩾`    | `a b -- (a ⩾ b)`    | no          |

## Arrays

| Symbol | Definition         | Commutative |
|---     |---                 |---          |
| `∪`    | `A B -- (A ∪ B)`   | yes         |
| `+`    | `a A -- (A ∪ {a})` | yes         |
| `*`    | `A B -- (A * B)`   | no          |

## Constants

| Symbol | Type     | Value     |
|---     |---       |---        |
| `α`    | `bool`   | `true`    |
| `β`    | `bool`   | `false`   |
| `ε`    | `string` | `""`      |
| `π`    | `float`  | `3.14...` |
| `∅`    | `array`  | `{}`      |