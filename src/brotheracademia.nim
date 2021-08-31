import brotheracademia/[parser, tokenizer, expressions, runtime]
export parser, tokenizer, expressions, runtime

echo parse("combination(n: Int, r: Int) = \\for result x = 1, each i in 0..<r do while i < r do x = x * (n - i) / (r - i)")
echo parse("`for` a = b, c = d do e = f")
echo parse("a := b + c * 4")
