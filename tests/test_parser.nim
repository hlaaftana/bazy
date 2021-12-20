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
    "1a": "(1a)",
    "1ea": "(1ea)",
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
    "permutation(n: Int, r: Int) = product n - r + 1 .. n":
      # command syntax with infixes
      "(permutation(n: Int, r: Int) = (product (((n - r) + 1) .. n)))",
    "factorial(n: Int) = permutation n, n":
      # consequence
      "((factorial(n: Int) = (permutation n)), n)",
    "a =\n  b": "(a = b)", # postfix expansion
    "\\(foo a, b)": "(foo a, b)"
  }

  for inp, outp in tests.items:
    let parsed = parse(inp)
    check $parsed == outp

test "equivalent syntax":
  let equivalents = {
    "combination(n: Int, r: Int) = do for result x = 1, each i in 0..<r do " &
      "while i < r do x = x * (n - i) / (r - i)":
        "combination(n: Int, r: Int) = for(result x = 1, each i in 0..<r) do " &
          "while i < r, x = x * (n - i) / (r - i)"
  }

  proc reduced(a: Expression): Expression

  proc reduced(a: seq[Expression]): seq[Expression] =
    result.newSeq(a.len)
    for i in 0 ..< a.len:
      result[i] = a[i].reduced

  proc reduced(a: Expression): Expression =
    const equivalentKinds = [
      {Name, Symbol},
      {OpenCall, Infix, Prefix, Postfix,
        PathCall, PathInfix, PathPrefix, PathPostfix},
      {Block, SemicolonBlock}
    ]

    result = a
    while result.kind == Wrapped: result = result.wrapped

    for ek in equivalentKinds:
      if result.kind in ek:
        if OpenCall in ek:
          result = Expression(kind: PathCall, address: result.address.reduced,
            arguments: result.arguments.reduced)
        elif Name in ek:
          result = Expression(kind: Name, identifier: result.identifier)
        elif Block in ek:
          result = Expression(kind: Block, statements: result.statements.reduced)
        else:
          echo "weird equivalent kinds set ", ek
        return

  for a, b in equivalents.items:
    let a = reduced(parse(a))
    let b = reduced(parse(b))
    checkpoint "a: " & $a
    checkpoint "b: " & $b
    check a == b

import bazy/tokenizer

when not defined(nimscript) and defined(testsBenchmark):
  import std/monotimes, strutils
  template bench(name, body) =
    let a = getMonoTime()
    body
    let b = getMonoTime()
    let time = formatFloat((b.ticks - a.ticks).float / 1_000_000, precision = 2)
    echo name, " took ", time, " ms"
else:
  template bench(name, body) = body

test "parse files without crashing":
  for f in [
    "concepts/arguments.ba",
    "concepts/badspec.ba",
    "concepts/tag.ba",
    "concepts/test.ba"
  ]:
    when defined(testsBenchmark): echo "file ", f
    bench("loading file"):
      let s = when declared(read): read(f) else: readFile(f)
    bench("tokenizing"):
      let ts = tokenize(s)
    bench("parsing"):
      let _ = parse(ts)

when defined(testsBenchmark):
  test "200x size file":
    let f = "concepts/badspec.ba"
    bench("loading file"):
      let s = repeat(when declared(read): read(f) else: readFile(f), 200)
    bench("tokenizing"):
      let ts = tokenize(s)
    bench("parsing"):
      let _ = parse(ts)
