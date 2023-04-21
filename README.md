# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a concatenative [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built with ~1000 lines of Rust.


Fibonacci sequence:

```
@ a 0
@ b 1

α ω (
    b δ print "\n" print ρ
    a + @ b ρ @ a ρ b 0 >
)
```

Fizz buzz:

```
@ n 1

α ω (
    n 3 % 0  = ? ("Fizz\n" print ρ)
    n 5 % 0  = ? ("Buzz\n" print ρ)
    n 15 % 0 = ? ("FizzBuzz\n" print ρ)
    n 1 + @ n δ 100 <
)
```

Logarithm:

```
λ log (
    @ n ρ
    @ b ρ

    0 n b > n b = + ω (
        1 + n b / @ n ρ
        n b > n b = +
    )
)
```

Square root:

```
λ sqrt (
    @ a δ
    2 / @ y ρ 0 0
    α ω (
        a y / y + 2 / @ y ρ 1 + δ 5 <
    )
    ρ ρ y
)
```

Absolute value:

```
λ abs (δ 0 < δ ? (σ! σ!) ? (σ))
```