when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import bazy, bazy/vm/[primitives, values, types, compilation, arrays]

test "type relation":
  check {Ty(Integer).match(Ty(Float)), Ty(Float).match(Ty(Integer))} == {tmNone}

test "compile success":
  template working(a) =
    discard compile(a)
  template failing(a) =
    check (
      try:
        discard compile(a)
        false
      except CompileError:
        true)
  working "1 + 1"
  failing "1 + 1.0"

test "eval values":
  check toValue(Template) == toValue(Template)

  let tests = {
    "a = \"abcd\"; a": toValue("abcd"), # breaks arc
    # gc bugs:
    "a = (b = do c = 1); [a, 2, b,  3, c]":
      toValue(@[toValue(1), toValue(2), toValue(1), toValue(3), toValue(1)]),
    "a = (b = do c = 1); (a, 2, b,  3, c, \"ab\")":
      toValue(toArray([toValue(1), toValue(2), toValue(1), toValue(3), toValue(1), toValue("ab")])),
    "a = (b = do c = 1); a + (b + 3) + c": toValue(6),
    "9 * (1 + 4) / 2 - 3f": toValue(19.5),
    "9 * (1 + 4) div 2 - 3": toValue(19),
    "foo(x) = x + 1; foo(3)": toValue(4),
    """
gcd(a: Int, b: Int): Int =
  if b == 0
    a
  \else
    gcd(b, a mod b)
gcd(12, 42)
""": toValue(6),
    "foo(x) = x + 1; foo(x: Float) = x - 1.0; foo(3)": toValue(4),
    "foo(x) = x + 1; foo(x: Int) = x - 1; foo(3)": toValue(2),
    "foo(x: Float) = x - 1.0; foo(x) = x + 1; foo(3)": toValue(4),
    "foo(x: Int) = x - 1; foo(x) = x + 1; foo(3)": toValue(2),
    "foo(x) = x + 1; foo(x) = x - 1; foo(3)": toValue(2),
    "foo(x: Int) = if(x == 0, (), (x, foo(x - 1))); foo(5)":
      toValue(toArray([toValue 5,
        toValue(toArray([toValue 4,
          toValue(toArray([toValue 3,
            toValue(toArray([toValue 2,
              toValue(toArray([toValue 1,
                #[Value(kind: vkNone)]#toValue(toArray[Value]([]))]))]))]))]))])),
    "a = 1; foo() = a; foo()": toValue(1),
    """
foo(i: Int) =
  if i == 1
    "a"
  \else if i == 2
    "b"
  \else
    "c"
[foo(0), foo(1), foo(2), foo(3)]
""": toValue(@[toValue("c"), toValue("a"), toValue("b"), toValue("c")]),
    """
fibo(i: Int): Int =
  if i == 1
    1
  \else if i == 2
    1
  \else
    fibo(i - 1) + fibo(i - 2)

[fibo(3), fibo(4), fibo(5)]""": toValue(@[toValue(2), toValue(3), toValue(5)])
  }
  
  for inp, outp in tests.items:
    try:
      check evaluate(inp) == outp
    except:
      echo "fail: ", (input: inp)
      echo parse(inp)
      if getCurrentException() of ref NoOverloadFoundError:
        echo (ref NoOverloadFoundError)(getCurrentException()).scope.variables
      raise
