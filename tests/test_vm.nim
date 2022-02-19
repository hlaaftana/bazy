when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import bazy, bazy/vm/[primitives, values, types, compilation, arrays]

test "type relation":
  check {Ty(Integer).match(Ty(Float)), Ty(Float).match(Ty(Integer))} == {tmNone}
  let a1 = Type(kind: tyTuple, elements: @[Ty(Scope)], varargs: toRef(Ty(Expression)))
  let a2 = Type(kind: tyTuple, elements: @[Ty(Scope), Ty(Expression), Ty(Expression)])
  check {a1.match(a2), a2.match(a1)} == {tmAlmostEqual}
  let a3 = Type(kind: tyTuple, elements: @[Ty(Scope)], varargs: toRef(Ty(Any)))
  let a4 = Type(kind: tyTuple, elements: @[Ty(Scope)])
  check a1.match(a3) == tmFalse
  check a3.match(a1) == tmTrue
  check {a1.match(a4), a4.match(a1), a3.match(a4), a4.match(a3)} == {tmAlmostEqual}
  check a1 < a3

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
  working "while true, ()"
  failing "while 1.0, ()"

test "eval values":
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
  else
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
  else if i == 2
    "b"
  else
    "c"
[foo(0), foo(1), foo(2), foo(3)]
""": toValue(@[toValue("c"), toValue("a"), toValue("b"), toValue("c")]),
    """
fibo(i: Int): Int =
  if i == 1
    1
  else if i == 2
    1
  else
    fibo(i - 1) + fibo(i - 2)

[fibo(3), fibo(4), fibo(5)]""": toValue(@[toValue(2), toValue(3), toValue(5)]),
    "a = 1; foo() = (b = 2; bar() = (c = 3; (a, b, c)); bar()); foo()": toValue(toArray([toValue(1), toValue(2), toValue(3)])),
    """
foo() =
  x = 1
  (getter: (() => x),
  setter: ((y: Int) => x = y)) 
a = foo()
_ = a.setter.(3)
a.getter.()""": toValue(3),
    """
foo() =
  x = 1
  (getter: (() => x),
  setter: ((y: Int,) => x := y)) 
a = foo()
_ = a.setter.(3)
_ = a.getter.()""": toValue(1),
    """
foo() =
  x = 1
  (getter: (
    () => x),
  setter: (
    (y: Int) => x = y)) 
static a = foo()
_ = a.setter.(3)
a.getter.()""": toValue(3),
    """
foo() =
  x = 1
  (getter: (() => x),
  setter: ((y: Int) => x = y)) 
static
  a = foo()
  _ = a.setter.(3)
a.getter.()""": toValue(3),
    "(true and false, true and true, false and true, false and false)": toValue(toArray([toValue(false), toValue(true), toValue(false), toValue(false)])),
    """
a = 0
(true and (a = a + 1; false), true and (a = a + 1; true), false and (a = a + 1; true), false and (a = a + 1; false), a)""": toValue(toArray([toValue(false), toValue(true), toValue(false), toValue(false), toValue(2)])),
    "i = 5; while(i > 0, i = i - 1); i": toValue(0),
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

import bazy/vm/library/common

module withVarargsFn:
  define "max", funcTypeWithVarargs(Ty(Integer), [], Ty(Integer)), (doFn do:
    var res = args[0].integerValue
    for i in 1 ..< args.len:
      let el = args[i].integerValue
      if el > res: res = el
    toValue(res))

test "varargs function":
  let libraries = @[Prelude, withVarargsFn()]
  let compiled = compile("""
max(3, 7, 4, 5)
""", libraries)
  check compiled.run() == toValue(7)
  check:
    try:
      discard compile("max(3.0, 4.0, 5.0)", libraries)
      false
    except NoOverloadFoundError:
      true
