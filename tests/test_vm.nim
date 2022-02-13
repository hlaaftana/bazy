when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import bazy, bazy/vm/[primitives, values, types, compilation, arrays]

test "type relation":
  check {makeType(Integer).match(makeType(Float))
, makeType(Float).match(makeType(Integer))} == {tmNone}

# XXX insane GC bugs

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
  # useMalloc stops immediately:
  working "1 + 1"
  failing "1 + 1.0"

test "eval values":
  check toValue(Template) == toValue(Template)

  let tests = {
    "a = \"abcd\"; a": toValue("abcd"), # breaks arc
    # gc bugs:
    #"a = (b = do c = 1); [a, 2, b,  3, c]":
    #  toValue(@[toValue(1), toValue(2), toValue(1), toValue(3), toValue(1)]),
    #"a = (b = do c = 1); (a, 2, b,  3, c, \"ab\")":
    #  toValue(toShortArray([toValue(1), toValue(2), toValue(1), toValue(3), toValue(1), toValue("ab")])),
    "a = (b = do c = 1); a + (b + 3) + c": toValue(6),
    "9 * (1 + 4) / 2 - 3f": toValue(19.5),
    "9 * (1 + 4) div 2 - 3": toValue(19),
  }
  
  for inp, outp in tests.items:
    check evaluate(inp) == outp
