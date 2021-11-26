import tokenizer, expressions, operators

type
  ParserOptions* = object
    curlyBlocks*, operatorIndentMakesBlock*: bool

  Parser* = object
    tokens*: seq[Token]
    pos*: int
    options*: ParserOptions

proc defaultOptions*(): ParserOptions =
  ParserOptions(curlyBlocks: false, operatorIndentMakesBlock: true)

iterator nextTokens*(p: var Parser): Token =
  while p.pos < p.tokens.len:
    yield p.tokens[p.pos]
    inc p.pos

#[

notes:

* probably a lot of indent bugs when you indent in parentheses or with variable spaces
* some kind of delimiter counter/stack on parser object would stop infinite loops from errors
* maybe make distinction between indent and normal argument, or `do` and indent, no idea
* maybe postfix statements like if/for/while can end the line and record a new line for the RHS,
  or maybe some god operator like |-> to invoke these, dont know a not ugly operator though

]#

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
  if semicoloned: Expression(kind: SemicolonBlock, statements: s)
  elif s.len == 0: Expression(kind: ExpressionKind.None)
  else: s[0]

proc recordBlockLevel*(parser: var Parser, indented = false): Expression =
  result = Expression(kind: Block)
  var backslashNames: seq[string]
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace, tkIndent, tkNewline: continue
    of tkSemicolon: discard
    of tkIndentBack:
      assert indented, "should not receive indent back at top level"
      break
    of tkComma, tkCloseBrack, tkCloseCurly, tkCloseParen:
      assert false, "wrong delim in block"
    elif token.kind == tkColon and result.statements.len > 0 and
      result.statements[^1].kind in {OpenCall, PathCall, Infix, Prefix, Postfix}:
      inc parser.pos
      result.statements[^1].arguments.add(parser.recordWideLine())
      #[if tok.kind == tkWord:
        inc parser.pos
        result.statements[^1].arguments.add(Expression(kind: ExpressionKind.Colon,
          left: Expression(kind: Name, identifier: tok.raw),
          right: parser.recordWideLine()))]#
    elif token.kind == tkBackslash and result.statements.len > 0 and
      result.statements[^1].kind in {OpenCall, PathCall, Infix, Prefix, Postfix} and (
        var lastEx = result.statements[^1]
        var valid = true
        for n in backslashNames:
          if n == "" and lastEx.arguments.len > 0 and
            lastEx.arguments[^1].kind in {OpenCall, PathCall, Infix, Prefix, Postfix}:
            lastEx = lastEx.arguments[^1]
          elif n != "" and lastEx.arguments.len > 0 and
            lastEx.arguments[^1].kind == ExpressionKind.Colon and
            lastEx.arguments[^1].left.kind == Name and lastEx.arguments[^1].left.identifier == n and
            lastEx.arguments[^1].right.kind in {OpenCall, PathCall, Infix, Prefix, Postfix}:
            lastEx = lastEx.arguments[^1].right
          else:
            valid = false
            break
        valid): # epic abuse
      inc parser.pos
      let ex = lastEx
      let tok = parser.tokens[parser.pos]
      if tok.kind == tkWord:
        inc parser.pos
        if parser.tokens[parser.pos].kind == tkNewLine: inc parser.pos
        let name = tok.raw
        ex.arguments.add(Expression(kind: ExpressionKind.Colon,
          left: Expression(kind: Name, identifier: name),
          right: parser.recordWideLine()))
        backslashNames.add(name)
      else:
        if parser.tokens[parser.pos].kind == tkNewLine: inc parser.pos
        ex.arguments.add(parser.recordWideLine())
        backslashNames.add("")
      #dec parser.pos
      #^ why was this here? it broke everything
      continue
    else:
      result.statements.add(parser.recordWideLine())
    reset backslashNames
  if result.statements.len == 1: result = result.statements[0]

proc recordBrack*(parser: var Parser): Expression =
  var s: seq[Expression]
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline, tkComma: discard
    of tkCloseBrack:
      break
    of tkCloseCurly, tkCloseParen:
      assert false, "wrong delim for brack"
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
      assert false, "wrong delim for paren"
    else:
      s.add(parser.recordWideLine(true))
  if not commad and s.len == 1: Expression(kind: Wrapped, wrapped: s[0])
  else: Expression(kind: Tuple, elements: s)

proc recordCurly*(parser: var Parser): Expression =
  var s: seq[Expression]
  var makeSet = not parser.options.curlyBlocks
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline: discard
    of tkComma: makeSet = true
    of tkCloseCurly:
      break
    of tkCloseBrack, tkCloseParen:
      assert false, "wrong delim for curly"
    else:
      s.add(parser.recordWideLine(makeSet))
  if makeSet: Expression(kind: Set, elements: s)
  else: Expression(kind: Block, statements: s)

proc recordSingle*(parser: var Parser): Expression =
  # +a is a path
  # a.b[c] is a path
  # a+b/c is a path and implies (a + b) / c
  var precedingSymbol: Expression
  var precedingDot = false
  var currentSymbol: Expression
  var lastWhitespace: bool
  var lastDot: bool
  defer:
    dec parser.pos
    let resultWasNil = result.isNil
    if not currentSymbol.isNil:
      if resultWasNil:
        result = currentSymbol
      else:
        result = Expression(kind: PathPostfix, address: currentSymbol, arguments: @[result])
    if not precedingSymbol.isNil and not resultWasNil:
      result = Expression(kind: PathPrefix, address: precedingSymbol, arguments: @[result])
    if precedingDot:
      result = Expression(kind: Dot,
        left: Expression(kind: ExpressionKind.None),
        right: result)
  template finish = return
  for token in parser.nextTokens:
    case token.kind
    of tkNone: discard
    of tkWhitespace:
      if not currentSymbol.isNil:
        finish()
      lastWhitespace = true
    of tkIndent, tkIndentBack, tkNewLine,
       tkComma, tkSemicolon, tkCloseParen,
       tkCloseCurly, tkCloseBrack, tkColon:
      finish()
    of tkString, tkNumber, tkWord:
      let ex = case token.kind
        of tkString: Expression(kind: String, str: token.content)
        of tkNumber: Expression(kind: Number, number: token.num)
        of tkWord: Expression(kind: Name, identifier: token.raw)
        else: nil
      if result.isNil:
        result = ex
        if lastDot:
          precedingDot = true
        if not currentSymbol.isNil:
          precedingSymbol = currentSymbol
          currentSymbol = nil
      elif lastDot:
        result = Expression(kind: Dot, left: result, right: ex)
      elif not currentSymbol.isNil:
        result = Expression(kind: PathInfix, address: currentSymbol, arguments: @[result, ex])
        currentSymbol = nil
      else: # lastWhitespace
        finish()
      lastWhitespace = false
      lastDot = false
    of tkOpenBrack, tkOpenCurly, tkOpenParen:
      if lastWhitespace and not lastDot:
        finish()
      inc parser.pos
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
            result = Expression(kind: PathCall, address: result.right, arguments: @[result.left] & move ex.elements)
          else:
            result = Expression(kind: PathCall, address: result, arguments: move ex.elements)
        of Array: result = Expression(kind: Subscript, address: result, arguments: move ex.elements)
        of Set: result = Expression(kind: CurlySubscript, address: result, arguments: move ex.elements)
        elif result.kind == Dot: # single expression
          result = Expression(kind: PathCall, address: result.right, arguments: @[result.left, ex])
        else:
          result = Expression(kind: PathCall, address: result, arguments: @[ex])
      else:
        result = Expression(kind: PathInfix, address: currentSymbol, arguments: @[result, ex])
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

proc collectLineExpression*(exprs: seq[Expression]): Expression =
  if exprs.len == 0: return Expression(kind: ExpressionKind.None)
  var terms = reduceOperators(exprs)
  result = terms.pop
  while terms.len != 0:
    let callee = terms.pop
    if callee.kind in {Prefix, Infix}: # postfix should not be possible
      var deepest = callee
      while deepest.arguments[^1].kind in {Prefix, Infix}:
        deepest = deepest.arguments[^1]
      deepest.arguments[^1] = Expression(kind: OpenCall, address: deepest.arguments[^1], arguments: @[result])
      result = callee
    else:
      result = Expression(kind: OpenCall, address: callee, arguments: @[result])

proc recordLineLevel*(parser: var Parser, closed = false): Expression =
  type CommaKind = enum
    NoComma, CommaCall, CommaList
  var expressions: seq[Expression]
  var comma: CommaKind
  var waiting = false
  var currentExprs: seq[Expression]
  var indent: Expression
  var indentIsDo: bool
  defer: # rather defer than put this at the end and try to put `break` everywhere
    dec parser.pos
    if currentExprs.len != 0:
      expressions.add(currentExprs.collectLineExpression())
      reset currentExprs
    if not indent.isNil:
      if (parser.options.operatorIndentMakesBlock or indentIsDo) and
        expressions.len == 1 and expressions[0].kind in {Prefix, Infix, Postfix, OpenCall, PathCall}:
        expressions[0].arguments.add(indent)
      else:
        expressions.add(indent)
    if expressions.len == 0:
      result = Expression(kind: ExpressionKind.None)
    elif comma == CommaList:
      result = Expression(kind: Tuple, elements: expressions)
    elif expressions.len == 1:
      result = expressions[0]
    else:
      result = Expression(kind: OpenCall, address: expressions[0], arguments: expressions[1..^1])
  template finish = return
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace: discard
    of tkComma:
      if closed:
        finish()
      waiting = true
      if currentExprs.len != 0: # consider ,, typo as ,
        let ex = collectLineExpression(currentExprs)
        if comma == NoComma and ex.kind == OpenCall:
          assert ex.arguments.len == 1, "opencall with more than 1 argument should be impossible before comma"
          comma = CommaCall
          expressions.add(ex.address)
          expressions.add(ex.arguments)
        else:
          if comma == NoComma: comma = CommaList
          expressions.add(ex)
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
              inc parser.pos
              indent = parser.recordBlockLevel(indented = true)
              finish()
          else:
            dec parser.pos
            finish()
      waiting = false
    of tkIndentBack:
      if not waiting or closed:
        finish()
    of tkSemicolon, tkCloseBrack, tkCloseCurly, tkCloseParen:
      finish()
    of tkBackslash:
      inc parser.pos
      currentExprs.add(parser.recordLineLevel(closed))
      finish()
    of tkColon:
      waiting = true
      currentExprs.add(Expression(kind: Symbol, identifier: ":"))
    of tkIndent:
      if not waiting:
        inc parser.pos
        indent = parser.recordBlockLevel(indented = true)
        indentIsDo = false
        finish()
    elif (token.kind == tkSymbol and token.raw == "do"):
      waiting = false
      inc parser.pos, 2
      indent = parser.recordWideLine()
      indentIsDo = true
      finish()
    else:
      let ex = parser.recordSingle()
      waiting = false # symbols do not bypass this
      currentExprs.add(ex)
  finish()

proc recordBlockLevel*(tokens: seq[Token]): Expression =
  var parser = Parser(tokens: tokens, pos: 0, options: defaultOptions())
  result = parser.recordBlockLevel()

proc parse*(str: string): Expression =
  var tokenizer = Tokenizer(str: str, pos: 0)
  tokenizer.symbolWords = @[
    "do", "and", "or", "is", "as", "not", "in", "div", "mod", "xor"
  ]
  result = parser.recordBlockLevel(tokenizer.tokenize())

when isMainModule:
  let tests = [
    readFile("concepts/test.ba"),
    #readFile("concepts/tag.ba"),
    #readFile("concepts/arguments.ba"),
    #readFile("concepts/badspec.ba")
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
