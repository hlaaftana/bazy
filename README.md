# Kismet

A programming language. Started in 2016 or 2017.

## Syntax

```
;; single line comment

abc     ;; identifier, any character that isn't .[](){},:; or whitespace
`abc`   ;; quoted identifier, can be anything
123     ;; number, syntax is in the air
"abc"   ;; string

[foo a b]  ;; calls foo with arguments a, b
foo a b    ;; line call, calls foo with arguments a and b
[foo a
     b]    ;; same as [foo a b]
foo a
    b      ;; same as [foo a] then b

foo a b; bar a b      ;; calls [foo a b] first then [bar a b]
[foo a b] [bar a b]   ;; special: calls [foo a b] first then [bar a b]
[[foo a b] [bar a b]] ;; calls the value of [foo a b] with [bar a b]

(foo a b)             ;; [foo a b]
(foo a
     b)               ;; [foo a b]
(foo a b; bar a b)    ;; [foo a b] [bar a b]
(foo a b) (bar a b)   ;; [[foo a b] [bar a b]]
([foo a b] [bar a b]) ;; [[foo a b] [bar a b]]
((foo a b) (bar a b)) ;; [[foo a b] [bar a b]]

{foo a b}             ;; same as [foo a b]
{
  foo a b
  bar a b
}                     ;; [foo a b] [bar a b]

a.b      ;; gets property b of a
a . b    ;; a.b
a.[b]    ;; accesses a with value b
a[b]     ;; a.[b], no character between a and [
a.(b)    ;; [a b]
a.(b, c) ;; [a b c]
a(b)     ;; [a b]
a(b, c)  ;; [a b c]
a.b(c)   ;; [b a c]
a:b      ;; sets variable a to b

[a, b]   ;; creates list of a and b
[a,]     ;; creates list of just a
(a, b)   ;; creates tuple of a and b
(a,)     ;; creates tuple of just a
{a, b}   ;; creates set of a and b
{a,}     ;; creates set of just a
{: a: b}  ;; creates map with an entry a -> b
{:a}     ;; creates map with an entry a -> a
```

## Program example

```
let (each i: range 1 100) {
  tu: reduce (0, 5, 3) [fn [a b]
    [bit_and 
      [left_shift a 1]
      [if [divs? i b] 1 0]]]
  echo [i, "Fizz", "Buzz", "FizzBuzz"][tu]
}
```