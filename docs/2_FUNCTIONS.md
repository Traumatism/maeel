
We define the function `foo` which parameters are `a` and `b` using:

```
fun foo a b (

)
```

To call `foo`, we use:

```
1 -> a
2 -> b

a b foo
```

Or more simply:

```
1 2 foo
```

To avoid recursive function call stack overflow, we can use inline functions by adding the keyword `inline` after `fun`:

```
fun inline foo a b (
    a b foo
)
```
