import unittest

import brotheracademia/[parser, expressions]

test "simple code":
  let tests = {
    "a": "a",
    "a.b": "a.b",
    "a+b": "(a + b)",
    "a + b": "(a + b)",
    "a +b": "(a (+ b))",
    "a+ b": "((a +) b)",
    "a.b+c/2": "((a.b + c) / 2)",
    "a(b, c)": "a(b, c)",
    "a * b / c ^ d ^ e << f | g + h < i as j":
      "((((a * b) / (c ^ (d ^ e))) << ((f | g) + h)) < (i as j))",
    "a do\n  b": "(a b)",
    "a\n  b\n  c\nd\ne\n  f\n    g\nh": """(
  (a (
    b;
    c
  ));
  d;
  (e (f g));
  h
)""",
    "a b, c": "(a b, c)",
    "a b c, d e, f": "(a (b c), (d e), f)",
    "a b\n\\c\n  d\n  e": """(a b, c: (
  d;
  e
))""",
    "a = \\b c\na = \\b c\n  a = \\b c\na = \\b c": """(
  (a = (b c));
  (a = (b c, (a = (b c))));
  (a = (b c))
)""",
    """
    a = (b = \c d
      e = \f g
    h = \i j)""": "(a = ((b = (c (d (e = (f (g (h = (i j))))))))))",
  }

  for inp, outp in tests.items:
    check $parse(inp) == outp
