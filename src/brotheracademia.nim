import brotheracademia/[parser, expressions, runtime]

echo parse("combination(n: Int, r: Int) = \\for result x = 1, each i in 0..<r do while i < r do x = x * (n - i) / (r - i)")
