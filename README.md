# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a concatenative [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built with ~1000 lines of Rust.


### abs(x)

```
λ abs (δ 0 < δ ⟹ (σ! σ!) ⟹ (σ))
```

### sqrt(x)

```
λ sqrt (
    δ → a 2 / → y 0 0
    α ω (a y / y + 2 / → y ↑ δ 5 <)
    ρ ρ y
)

2 sqrt print
```

### factorial(n)

```


λ fact (
    1 → f δ → n 0 > ω (f n * → f n ↓ δ → n 0 >) f
)

7 fact print
```

### Fibonacci sequence (Fn)

```
λ fib (
    0 1 1 → b → i → a → n
    i n ≤ ω (a b + → c b → a c → b i ↑ → i i n ≤)
    a
)

7 fib print
```

### Get primes

```
λ wilson_theorem (
    1 → f → n 2 2 n < ω
    (δ f * n % → f ↑ δ n <)
    n ↓ f =
)

2 α ω (
    ↑ δ wilson_theorem ⟹ (δ print ρ "\n" print ρ)
    α
)
```