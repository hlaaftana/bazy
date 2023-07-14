when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import bazy, bazy/vm/[primitives, values, types, compilation, arrays]

test "type relation":
  check {Ty(Int32).match(Ty(Float32)), Ty(Float32).match(Ty(Int32))} == {tmNone}
  let a1 = Type(kind: tyTuple, elements: @[Ty(Scope)], varargs: box Ty(Expression))
  let a2 = Type(kind: tyTuple, elements: @[Ty(Scope), Ty(Expression), Ty(Expression)])
  check {a1.match(a2), a2.match(a1)} == {tmAlmostEqual}
  let a3 = Type(kind: tyTuple, elements: @[Ty(Scope)], varargs: box Ty(Any))
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

    # functions:
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

    # closures: xxx (5) mutable variables should be reference
    "a = 1; foo() = a; foo()": toValue(1),
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
    # xxx not working, gives 1 for a
    "1": when true: toValue(1) else: {"""
foo() =
  x = 1
  (getter: (() => x),
  setter: ((y: Int) => x = y)) 
a = foo()
_ = a.setter.(3)
b = foo()
[a.getter.(), b.getter.()]""": toValue(@[toValue(3), toValue(1)]),
    """
foo() =
  x = 1
  (getter: (
    () => x),
  setter: (
    (y: Int) => x = y)) 
static a = foo()
_ = a.setter.(3)
static b = foo()
c = foo()
[a.getter.(), b.getter.(), c.getter.()]""": toValue(@[toValue(3), toValue(1), toValue(1)]),
    """
foo() =
  x = 1
  (getter: (() => x),
  setter: ((y: Int) => x = y)) 
static
  a = foo()
  _ = a.setter.(3)
  b = foo()
c = foo()
[a.getter.(), b.getter.(), c.getter.()]""": toValue(@[toValue(3), toValue(1), toValue(1)])
    },

    "(true and false, true and true, false and true, false and false)": toValue(toArray([toValue(false), toValue(true), toValue(false), toValue(false)])),
    """
a = 0
(true and (a = a + 1; false), true and (a = a + 1; true), false and (a = a + 1; true), false and (a = a + 1; false), a)""": toValue(toArray([toValue(false), toValue(true), toValue(false), toValue(false), toValue(2)])),
    "i = 5; while(i > 0, i = i - 1); i": toValue(0),
    "1 + 1 == 1 or false": toValue(false),
    "(1, (2, 3), 4)[1][0]": toValue(2),
    "a = (1, (2, 3), 4); a[0] == 1 and a[1][1] == 3": toValue(true),
    "`.[]=`(a: Int, b: Int, c: Int) = a * b + c; 3[4] = 5": toValue(17),
    # also tests generics:
    "a = ref 1; update a, 1 + unref a; a": Value(kind: vkReference, referenceValue: toValue(2).toFullValueObj.toRef),

    # closures with references (mutable):
    "a = ref 1; foo() = unref a; [foo(), (update(a, 2); foo()), foo()]": toValue(@[toValue(1), toValue(2), toValue(2)]),
    #"a = 1; foo() = (b = 2; bar() = (c = 3; (a, b, c)); bar()); foo()": toValue(toArray([toValue(1), toValue(2), toValue(3)])),
    """
foo() =
  x = ref 1
  (getter: (() => unref x),
  setter: ((y: Int) => update(x, y))) 
a = foo()
_ = a.setter.(3)
a.getter.()""": toValue(3),
    """
foo() =
  x = ref 1
  (getter: (
    () => unref x),
  setter: (
    (y: Int) => update(x, y))) 
static a = foo()
_ = a.setter.(3)
a.getter.()""": toValue(3),
    """
foo() =
  x = ref 1
  (getter: (() => unref x),
  setter: ((y: Int) => update(x, y))) 
static
  a = foo()
  _ = a.setter.(3)
a.getter.()""": toValue(3),
    # XXX (5) not working yet (a gives 1):
    "1": when true: toValue(1) else: {
    """
foo() =
  x = ref 1
  (getter: (() => unref x),
  setter: ((y: Int) => update(x, y))) 
a = foo()
_ = a.setter.(3)
b = foo()
[a.getter.(), b.getter.()]""": toValue(@[toValue(3), toValue(1)]),
    """
foo() =
  x = ref 1
  (getter: (
    () => unref x),
  setter: (
    (y: Int) => update(x, y))) 
static a = foo()
_ = a.setter.(3)
static b = foo()
c = foo()
[a.getter.(), b.getter.(), c.getter.()]""": toValue(@[toValue(3), toValue(1), toValue(1)]),
    """
foo() =
  x = ref 1
  (getter: (() => unref x),
  setter: ((y: Int) => update(x, y))) 
static
  a = foo()
  _ = a.setter.(3)
  b = foo()
c = foo()
[a.getter.(), b.getter.(), c.getter.()]""": toValue(@[toValue(3), toValue(1), toValue(1)])
    },
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
  define "max", funcTypeWithVarargs(Ty(Int32), [], Ty(Int32)), (doFn do:
    var res = args[0].int32Value
    for i in 1 ..< args.len:
      let el = args[i].int32Value
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

module withGeneric:
  let T = Type(kind: tyParameter, parameter: newTypeParameter("T"))
  let f = define(result, "foo", funcType(Type(kind: tyList, elementType: box T), [T]))
  f.genericParams = @[T.parameter]
  result.top.define(f)
  let f2 = define(result, "foo", funcType(Type(kind: tyList, elementType: box Ty(Int32)), [Ty(Int32)]))
  result.top.define(f2)
  result.refreshStack()
  result.stack.set f.stackIndex, toValue proc (args: openarray[Value]): Value =
    result = toValue(@[args[0]])
  result.stack.set f2.stackIndex, toValue proc (args: openarray[Value]): Value =
    result = toValue(@[toValue(-args[0].int32Value)])

test "generic":
  let libraries = @[Prelude, withGeneric()]
  let compiled = compile("""
(foo(123), foo("abc"))
""", libraries)
  check compiled.run() == toValue(toArray([toValue(@[toValue(-123)]), toValue(@[toValue("abc")])]))

module withGenericMeta:
  let T = Type(kind: tyParameter, parameter: newTypeParameter("T"))
  let f = define(result, "foo", funcType(Ty(Statement), [Ty(Scope), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(Type(kind: tyList, elementType: box T), [T]))
  ))
  f.genericParams = @[T.parameter]
  result.top.define(f)
  let f2 = define(result, "foo", funcType(Ty(Statement), [Ty(Scope), Ty(Statement)]).withProperties(
    property(Meta, toValue funcType(Type(kind: tyList, elementType: box Ty(Int32)), [Ty(Int32)]))
  ))
  result.top.define(f2)
  result.refreshStack()
  result.stack.set f.stackIndex, toValue proc (args: openarray[Value]): Value =
    let scope = args[0].boxedValue.scopeValue
    result = toValue compile(scope, Expression(kind: Array,
      elements: @[args[1].boxedValue.expressionValue]), +Ty(Any))
  result.stack.set f2.stackIndex, toValue proc (args: openarray[Value]): Value =
    result = toValue Statement(kind: skList,
      knownType: Type(kind: tyList, elementType: box Ty(Int32)),
      elements: @[Statement(kind: skUnaryInstruction,
        knownType: Ty(Int32),
        unaryInstructionKind: NegInt,
        unary: args[1].boxedValue.statementValue.toInstruction)])

test "generic meta":
  let libraries = @[Prelude, withGenericMeta()]
  let compiled = compile("""
(foo(123), foo("abc"))
""", libraries)
  check compiled.run() == toValue(toArray([toValue(@[toValue(-123)]), toValue(@[toValue("abc")])]))
