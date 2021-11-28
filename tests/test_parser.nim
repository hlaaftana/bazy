when (compiles do: import nimbleutils/bridge):
  import nimbleutils/bridge
else:
  import unittest

import bazy/[parser, expressions]

test "simple code":
  let tests = {
    "a": "a",
    "a.b": "a.b",
    "a+b": "(a+b)",
    "a + b": "(a + b)",
    "a +b": "(a (+b))",
    "a+ b": "((a+) b)",
    "a.b+c/2": "((a.b+c)/2)",
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
    "\"abc\"": "\"abc\"",
    "1..20": "(1..20)",
    "1a": "(1 a)",
    "1ea": "(1 ea)",
    "+3.0 / -2.0": "(3.0 / -2.0)",
    "3e-2000 / 2e1400": "(3e-2000 / 2e1400)",
    "+3.1419e2 / -27.828e-1": "(314.19 / -2.7828)",
    "0.03": "0.03",
    "0.00042e-4": "0.00042e-4",
    "a + b c + d e": "(a + (b (c + (d e))))",
    "a+b c+d e": "((a+b) ((c+d) e))",
    "a + b c, d e + f g": "((a + (b c)), (d (e + (f g))))",
    "a do(b) do(c)": "(a ((b) (c)))",
    """
a = \b
  c = \d
    e = \f
      g = \h
  i = \j
k""": """(
  (a = (b (
    (c = (d (e = (f (g = h)))));
    (i = j)
  )));
  k
)""",
    """
a do b do (c)
d""": """(
  (a (b (c)));
  d
)""",
    """if a, b,
else: (do
  c
  d)""": """(if a, b, else: ((
  c;
  d
)))""",
    "permutation(n: Int, r: Int) = product n - r + 1 .. n": # command syntax with infixes
      "(permutation(n: Int, r: Int) = (product (((n - r) + 1) .. n)))",
    "a =\n  b": "(a = b)" # postfix expansion
  }

  for inp, outp in tests.items:
    check $parse(inp) == outp

test "parse files without crashing":
  for f in [
    "concepts/arguments.ba",
    "concepts/badspec.ba",
    "concepts/tag.ba",
    "concepts/test.ba"
  ]:
    discard parse((when declared(read): read else: readFile)(f))

test "equivalent syntax":
  let equivalents = {
    "combination(n: Int, r: Int) = do for result x = 1, each i in 0..<r do " &
      "while i < r do x = x * (n - i) / (r - i)":
        "combination(n: Int, r: Int) = for(result x = 1, each i in 0..<r) do " &
          "while i < r, x = x * (n - i) / (r - i)"
  }

  proc equivalent(a, b: Expression): bool

  proc equivalent(a, b: seq[Expression]): bool =
    if a.len != b.len: return false
    for i in 0 ..< a.len:
      if not equivalent(a[i], b[i]):
        return false
    true

  proc equivalent(a, b: Expression): bool =
    const equivalentKinds = [
      {Name, Symbol},
      {OpenCall, Infix, Prefix, Postfix,
        PathCall, PathInfix, PathPrefix, PathPostfix},
      {Block, SemicolonBlock}
    ]

    var (a, b) = (a, b)
    while a.kind == Wrapped: a = a.wrapped
    while b.kind == Wrapped: b = b.wrapped

    for ek in equivalentKinds:
      if {a.kind, b.kind} <= ek:
        if OpenCall in ek:
          result = equivalent(a.address, b.address) and equivalent(a.arguments, b.arguments)
        elif Name in ek:
          result = a.identifier == b.identifier
        elif Block in ek:
          result = equivalent(a.statements, b.statements)
        else:
          echo "weird equivalent kinds set ", ek
        return
    
    result = a == b

  for a, b in equivalents.items:
    check equivalent(parse(a), parse(b))
