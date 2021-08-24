import tokenizer, expressions, operators

type Parser* = object
  tokens*: seq[Token]
  pos*: int

# precedence enum to go down

iterator nextTokens*(p: var Parser): Token =
  while p.pos < p.tokens.len:
    yield p.tokens[p.pos]
    inc p.pos

proc recordLineLevel*(parser: var Parser, closed = false): Expression

proc recordWideLine*(parser: var Parser, closed = false): Expression =
  var s: seq[Expression]
  var semicoloned = false
  for t in parser.nextTokens:
    case t.kind
    of tkSemicolon:
      semicoloned = true
      #inc i
      #s.add(recordLine(p, true))
    of tkComma, tkCloseParen, tkCloseBrack, tkCloseCurly:
      dec parser.pos
      break
    of tkNewline:
      if not closed:
        dec parser.pos
        break
    of tkIndentBack:
      if not closed:
        break
    else:
      s.add(parser.recordLineLevel(closed))
  if semicoloned: Expression(kind: Block, statements: s)
  elif s.len == 0: Expression(kind: ExpressionKind.None)
  else: s[0]

proc recordBlockLevel*(parser: var Parser, indented = false): Expression =
  template finish =
    if result.statements.len == 1: result = result.statements[0]
    return
  result = Expression(kind: Block)
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace, tkIndent, tkNewline, tkSemicolon: discard
    of tkIndentBack:
      if indented: finish()
    of tkComma, tkCloseBrack, tkCloseCurly, tkCloseParen:
      discard # error unexpected token
    else:
      result.statements.add(parser.recordWideLine())
  finish()

proc recordBrack*(parser: var Parser): Expression =
  var s: seq[Expression]
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline, tkComma: discard
    of tkCloseBrack:
      break
    of tkCloseCurly, tkCloseParen:
      discard #error "error wwrong token for brack"
    else:
      s.add(parser.recordWideLine(true))
  Expression(kind: Array, elements: s)

proc recordParen*(parser: var Parser): Expression =
  var commad = false
  var s: seq[Expression]
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline: discard
    of tkComma: commad = true
    of tkCloseParen:
      break
    of tkCloseCurly, tkCloseBrack:
      discard #error "error wwrong token for brack"
    else:
      s.add(parser.recordWideLine(true))
  if not commad and s.len == 1: s[0]
  else: Expression(kind: Array, elements: s)

proc recordCurly*(parser: var Parser): Expression =
  var s: seq[Expression]
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline, tkComma: discard
    of tkCloseCurly:
      break
    of tkCloseBrack, tkCloseParen:
      discard #error "error wwrong token for brack"
    else:
      s.add(parser.recordWideLine(true))
  Expression(kind: Set, elements: s)

proc recordSingle*(parser: var Parser): Expression =
  # +a is a path
  # a.b[c] is a path
  # a+b/c is a path and implies (a + b) / c
  var precedingSymbol: Expression
  var precedingDot = false
  var currentSymbol: Expression
  var lastWhitespace: bool
  var lastDot: bool
  template finish =
    dec parser.pos
    let resultWasNil = result.isNil
    if not currentSymbol.isNil:
      if resultWasNil:
        result = currentSymbol
      else:
        result = Expression(kind: Postfix, address: currentSymbol, arguments: @[result])
    if precedingDot:
      discard # error no idea wtf preceding dot does
    if not precedingSymbol.isNil and not resultWasNil:
      result = Expression(kind: Prefix, address: precedingSymbol, arguments: @[result])
    return
  for token in parser.nextTokens:
    case token.kind
    of tkNone: discard
    of tkWhitespace:
      if not currentSymbol.isNil:
        finish()
      lastWhitespace = true
    of tkIndent, tkIndentBack, tkNewLine, tkComma, tkSemicolon, tkCloseParen, tkCloseCurly, tkCloseBrack, tkColon:
      finish()
    of tkString, tkNumber, tkWord:
      # account for subscripts somehow
      let ex = case token.kind
        of tkString: Expression(kind: String, str: token.content)
        of tkNumber: Expression(kind: Number, number: token.num)
        of tkWord: Expression(kind: Name, identifier: token.raw)
        else: nil
      if result.isNil:
        result = ex
        if lastDot:
          precedingDot = true
          lastDot = false
        if not currentSymbol.isNil:
          precedingSymbol = currentSymbol
          currentSymbol = nil
      elif lastDot:
        result = Expression(kind: Dot, left: result, right: ex)
        lastDot = false
      elif not currentSymbol.isNil:
        result = Expression(kind: Infix, address: currentSymbol, arguments: @[result, ex])
        currentSymbol = nil
      else: # lastWhitespace
        finish()
      lastWhitespace = false
    of tkOpenBrack, tkOpenCurly, tkOpenParen:
      inc parser.pos
      if lastWhitespace and not lastDot:
        finish()
      let ex = case token.kind
        of tkOpenBrack: parser.recordBrack()
        of tkOpenParen: parser.recordParen()
        of tkOpenCurly: parser.recordCurly()
        else: nil
      if result.isNil:
        result = ex
      elif currentSymbol.isNil:
        case ex.kind
        of Tuple:
          if result.kind == Dot:
            result = Expression(kind: Call, address: result.right, arguments: @[result.left] & ex.elements)
          else:
            result = Expression(kind: Call, address: result, arguments: ex.elements)
        of Array: result = Expression(kind: Subscript, address: result, arguments: ex.elements)
        of Set: result = Expression(kind: CurlySubscript, address: result, arguments: ex.elements)
        elif result.kind == Dot:
          result = Expression(kind: Call, address: result.right, arguments: @[result.left, ex])
        else:
          result = Expression(kind: Call, address: result, arguments: @[ex])
      else:
        result = Expression(kind: Infix, address: currentSymbol, arguments: @[result, ex])
        currentSymbol = nil
      lastDot = false
      lastWhitespace = false
    of tkDot:
      lastDot = true
      lastWhitespace = false
    of tkSymbol:
      let ex = Expression(kind: Symbol, identifier: token.raw)
      if lastDot:
        result = Expression(kind: Dot, left: result, right: ex)
      elif lastWhitespace:
        finish()
      else:
        currentSymbol = ex
        if result.isNil:
          precedingSymbol = currentSymbol
      lastDot = false
      lastWhitespace = false
    else: discard
  finish()

proc recordLineLevel*(parser: var Parser, closed = false): Expression =
  type CommaKind = enum
    NoComma, CommaCall, CommaList
  var expressions: seq[Expression]
  var comma: CommaKind
  var waiting = false
  var currentExprs: seq[Expression]
  template finish() =
    dec parser.pos
    if currentExprs.len != 0:
      var terms = reduceOperators(currentExprs)
      var expression = terms.pop
      while terms.len != 0:
        expression = Expression(kind: Call, address: terms.pop, arguments: @[expression])
      expressions.add(expression)
      reset currentExprs
    if expressions.len == 0:
      result = Expression(kind: ExpressionKind.None)
    elif comma == CommaList:
      result = Expression(kind: Tuple, elements: expressions)
    elif comma == NoComma and expressions.len == 1:
      result = expressions[0]
    else:
      result = Expression(kind: Call, address: expressions[0], arguments: expressions[1..^1])
    return
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace: discard
    of tkComma:
      waiting = true
      var terms = reduceOperators(currentExprs)
      echo terms
      let termLen = terms.len
      echo termLen
      var expression = terms.pop
      while terms.len != 0:
        expression = Expression(kind: Call, address: terms.pop, arguments: @[expression])
      if comma == NoComma:
        comma = if termLen > 1: CommaCall else: CommaList
      expressions.add(expression)
      reset currentExprs
    of tkNewline:
      if waiting or closed:
        for tok in parser.nextTokens:
          if tok.kind notin {tkNone, tkWhitespace, tkNewline, tkIndent, tkIndentBack}:
            dec parser.pos
            break
      else:
        for tok in parser.nextTokens:
          case tok.kind
          of tkNone, tkWhitespace, tkNewline: discard
          of tkIndent:
            if not waiting:
              if currentExprs.len != 0:
                var terms = reduceOperators(currentExprs)
                var expression = terms.pop
                while terms.len != 0:
                  expression = Expression(kind: Call, address: terms.pop, arguments: @[expression])
                expressions.add(expression)
                reset currentExprs
              expressions.add(parser.recordBlockLevel(indented = true))
          else:
            dec parser.pos
            finish()
      waiting = false
    of tkIndentBack, tkSemicolon, tkCloseBrack, tkCloseCurly, tkCloseParen:
      finish()
    of tkColon:
      waiting = true
      currentExprs.add(Expression(kind: Symbol, identifier: ":"))
    of tkIndent:
      if not waiting:
        if currentExprs.len != 0:
          var terms = reduceOperators(currentExprs)
          var expression = terms.pop
          while terms.len != 0:
            expression = Expression(kind: Call, address: terms.pop, arguments: @[expression])
          expressions.add(expression)
          reset currentExprs
        expressions.add(parser.recordBlockLevel(indented = true))
    elif (token.kind == tkSymbol and token.raw == "do"):
      waiting = false
      if currentExprs.len != 0:
        var terms = reduceOperators(currentExprs)
        var expression = terms.pop
        while terms.len != 0:
          expression = Expression(kind: Call, address: terms.pop, arguments: @[expression])
        expressions.add(expression)
        reset currentExprs
      inc parser.pos, 2
      expressions.add(parser.recordWideLine())
    else:
      let ex = parser.recordSingle()
      waiting = ex.kind == Symbol
      currentExprs.add(ex)
  finish()

proc recordBlockLevel*(tokens: seq[Token]): Expression =
  var parser = Parser(tokens: tokens, pos: 0)
  result = parser.recordBlockLevel()

proc parse*(str: string): Expression =
  var tokenizer = Tokenizer(str: str, pos: 0)
  tokenizer.symbolWords = @["do", "and", "or", "is", "as", "not"]
  result = parser.recordBlockLevel(tokenizer.tokenize())

when isMainModule:
  let tests = [
    "a",
    "a.b",
    "a+b",
    "a + b",
    "a +b",
    "a+ b",
    "a.b+c/2",
    "a(b, c)",
    "a * b / c ^ d ^ e << f | g + h < i as j",
    "a do\n  b",
    "a\n  b\n  c\nd\ne",
    readFile("concepts/test.ba")
  ]

  for t in tests:
    echo "input: ", t
    echo "output: ", parse(t)

  when false:
    import os

    for (k, p) in walkDir("concepts"):
      if k == pcFile:
        echo "file: ", p
        let e = parse(readFile(p))
        echo "expr: ", e
