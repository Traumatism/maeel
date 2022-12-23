# The maeel programming language

Just like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)), **maeel** is a [stack oriented programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) built in a single Rust file from scratch without any thirdparty library.

## hello, world !

```
"hello, world !" println
```

## sum/product of all integers from 1 to 50

```
let start 1
let end 50

"1x2x3x...x50=" print pop
start end range product println pop

"1+2+3+...+50=" print pop
start end range sum println pop
```

Note the program could have been written on a single line:
```
let start 1 let end 50 "1x2x3x...x50=" print pop start end range product println pop "1+2+3+...+50=" print pop start end range sum println pop
```

## manipulate strings

```
"hello" "world"
swap
2 take
" " join
println
```

## boolean logic / if conditions

```
let a true;
let b true; 

a b or if
    "a or b => true" println
    clear
;

a b and if
    "a and b => true" println
    clear
;

a b eq if
    "a eq b => true" println
    clear
;
```
