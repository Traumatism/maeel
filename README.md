# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a concatenative [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built with ~1000 lines of Rust.


### abs(x)

```
λ abs (δ 0 < δ ⟹ (σ! σ!) ⟹ (σ))
```

### sqrt(x)

```
λ sqrt (
    @ a δ 2 / @ y ρ 0 0
    α ω (a y / y + 2 / @ y ρ ↑ δ 5 <)
    ρ ρ y
)
```

### factorial(n)

```
λ fact (@n ρ @f 1 n 0 > ω (f n * @f ρ n ↓ @n ρ n 0 >) f)
```

### Fibonacci sequence (Fn)

```
λ fib (
    @n ρ @a 0 @b 1 @i 1
    i n < i n = + ω (
        a b + @c ρ b @a ρ c @b ρ
        i 1 + @i ρ i n < i n = +
    )
    a
)
```

### Get primes

```
λ wilson_theorem (
    @ n ρ @ f 1
    2 2 n < ω (δ f * n % @ f ρ ↑ δ n <)
    n ↓ f =
)

2 α ω (↑ δ wilson_theorem ⟹ (δ print "\n" print ρ ρ) α)
```