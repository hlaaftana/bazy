import tokens, expressions, operators

#[

notes:

* probably a lot of indent bugs when you indent in parentheses or with variable spaces
* some kind of delimiter counter/stack on parser object would stop infinite loops from errors
* keep track of indents with a counter
* maybe make distinction between indent and normal argument, or `do` and indent, no idea
* maybe postfix statements like if/for/while can end the line and record a new line for the RHS,
  or maybe some god operator like |-> to invoke these, dont know a not ugly operator though

]#

type
  ParserOptions* = object
    curlyBlocks*,
      pathOperators*,
      operatorIndentMakesBlock*,
      backslashLine*,
      backslashNestedPostArgument*,
      colonPostArgument*,
      weirdOperatorIndentUnwrap*,
      makeOperatorInfixOnIndent*,
      backslashParenLine*
    : bool
    defaultBackslashNames*: seq[string] # performance hazard

  Parser* = ref object
    tokens*: seq[Token]
    pos*: int
    fakeIndents*: int
    options*: ParserOptions

proc defaultOptions*(): ParserOptions =
  ParserOptions(
    curlyBlocks: false,
    pathOperators: true,
    operatorIndentMakesBlock: true,
    backslashLine: true, # should be false
    backslashNestedPostArgument: true,
    colonPostArgument: true,
    weirdOperatorIndentUnwrap: true,
    makeOperatorInfixOnIndent: true,
    backslashParenLine: true,
    defaultBackslashNames: @[#["else", "finally"]#])

proc newParser*(tokens: sink seq[Token] = @[], options = defaultOptions()): Parser =
  Parser(tokens: tokens, options: options)

iterator nextTokens*(p: var Parser): Token =
  while p.pos < p.tokens.len:
    let t = p.tokens[p.pos]
    if t.kind == tkIndentBack and p.fakeIndents > 0:
      dec p.fakeIndents
    else:
      yield t
    inc p.pos

proc makeStringExpression*(s: sink string): Expression {.inline.} =
  Expression(kind: String, str: unescape(s))

proc recordLineLevel*(parser: var Parser, closed = false): Expression

proc recordWideLine*(parser: var Parser, closed = false): Expression =
  var s: seq[Expression]
  var semicoloned = false
  for t in parser.nextTokens:
    case t.kind
    of tkSemicolon:
      semicoloned = true
    of tkComma, tkCloseParen, tkCloseBrack, tkCloseCurly:
      dec parser.pos
      break
    of tkNewline:
      if not closed:
        dec parser.pos
        break
    of tkIndentBack:
      if closed:
        dec parser.fakeIndents
      else:
        break
    else:
      s.add(parser.recordLineLevel(closed))
  if semicoloned: Expression(kind: SemicolonBlock, statements: s)
  elif s.len == 0: Expression(kind: ExpressionKind.None)
  else: s[0]

proc recordBlockLevel*(parser: var Parser, indented = false): Expression =
  result = Expression(kind: Block)
  defer:
    if result.statements.len == 1: result = result.statements[0]
  var backslashNames: seq[string]
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace, tkNewline: continue
    of tkSemicolon: discard
    of tkIndentBack:
      if indented:
        break
      else:
        dec parser.fakeIndents
        continue
    of tkComma, tkCloseBrack, tkCloseCurly, tkCloseParen:
      assert indented, "extra delim " & $token.kind
      dec parser.pos
      break
    elif token.kind == tkColon and 
      parser.options.colonPostArgument and
      result.statements.len > 0 and
      result.statements[^1].kind in IndentableCallKinds:
      inc parser.pos
      result.statements[^1].arguments.add(parser.recordWideLine())
    elif
      ((parser.options.backslashNestedPostArgument and token.kind == tkBackslash) or
        (parser.options.defaultBackslashNames.len != 0 and token.kind == tkWord and
          token.raw in parser.options.defaultBackslashNames)) and
      result.statements.len > 0 and
      result.statements[^1].kind in IndentableCallKinds and (
        # epic abuse
        var lastEx = result.statements[^1]
        var valid = true
        for n in backslashNames:
          if n == "" and lastEx.arguments.len > 0 and
            lastEx.arguments[^1].kind in IndentableCallKinds:
            lastEx = lastEx.arguments[^1]
          elif n != "" and lastEx.arguments.len > 0 and
            lastEx.arguments[^1].kind == ExpressionKind.Colon and
            lastEx.arguments[^1].left.kind == Name and lastEx.arguments[^1].left.identifier == n and
            lastEx.arguments[^1].right.kind in IndentableCallKinds:
            lastEx = lastEx.arguments[^1].right
          else:
            valid = false
            break
        valid):
      if token.kind == tkBackslash:
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
      continue
    else:
      result.statements.add(parser.recordWideLine())
    reset backslashNames

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
      s.add(parser.recordWideLine(closed = true))
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
      s.add(parser.recordWideLine(closed = true))
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
    # funny behavior
    elif t.kind == tkSemicolon and parser.options.curlyBlocks:
      makeSet = false
    else:
      s.add(parser.recordWideLine(closed = makeSet))
  if makeSet: Expression(kind: Set, elements: s)
  else: Expression(kind: Block, statements: s)

proc recordSingle*(parser: var Parser): Expression =
  # +a is a path
  # a.b[c] is a path
  # a+b/c is a path and implies (a + b) / c
  var
    precedingSymbol, currentSymbol: Expression = nil
    precedingDot, lastWhitespace, lastDot = false
  defer:
    dec parser.pos # paths will never terminate with delimiter
    let resultWasNil = result.isNil
    if not currentSymbol.isNil:
      if not parser.options.pathOperators:
        assert false, "currentSymbol should not exist when path operators are off"
      if resultWasNil:
        result = currentSymbol
      else:
        result = Expression(kind: PathPostfix, address: currentSymbol, arguments: @[result])
    if not precedingSymbol.isNil and not resultWasNil:
      if not parser.options.pathOperators:
        assert false, "precedingSymbol should not exist when path operators are off"
      result = Expression(kind: PathPrefix, address: precedingSymbol, arguments: @[result])
    if precedingDot:
      result = Expression(kind: PathPrefix,
        address: Expression(kind: Symbol, identifier: "."),
        arguments: @[result])
  template finish = return
  for token in parser.nextTokens:
    case token.kind
    of tkNone: discard
    of tkWhitespace:
      if not currentSymbol.isNil:
        finish()
      lastWhitespace = true
      continue
    of tkIndent, tkIndentBack, tkNewLine,
       tkComma, tkSemicolon, tkCloseParen,
       tkCloseCurly, tkCloseBrack, tkColon:
      finish()
    of tkString, tkNumber, tkWord:
      if lastWhitespace and not lastDot:
        finish()
      let ex = case token.kind
        of tkString:
          if lastDot:
            Expression(kind: String, str: unescape(token.content))
          else:
            Expression(kind: String, str: token.content)
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
      else:
        result = Expression(kind: PathPrefix, address: result, arguments: @[ex])
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
        if lastDot: precedingDot = true
      elif not currentSymbol.isNil:
        result = Expression(kind: PathInfix, address: currentSymbol, arguments: @[result, ex])
        currentSymbol = nil
      else:
        case ex.kind
        of Tuple:
          if result.kind == Dot:
            result = Expression(kind: PathCall, address: result.right, arguments: @[result.left] & move ex.elements)
          else:
            result = Expression(kind: PathCall, address: result, arguments: move ex.elements)
        of Array: result = Expression(kind: Subscript, address: result, arguments: move ex.elements)
        of Set: result = Expression(kind: CurlySubscript, address: result, arguments: move ex.elements)
        of Wrapped: # single expression
          if result.kind == Dot and not lastDot:
            result = Expression(kind: PathCall, address: result.right, arguments: @[result.left, ex.wrapped])
          else:
            result = Expression(kind: PathCall, address: result, arguments: @[ex.wrapped])
        else:
          assert false, "should return one of tuple, array, set, wrapped"
    of tkDot:
      lastDot = true
      lastWhitespace = false
      continue
    of tkSymbol, tkBackslash:
      if lastWhitespace and not lastDot:
        finish()
      let ex = Expression(kind: Symbol, identifier:
        if token.kind == tkSymbol: token.raw
        else: "\\")
      if result.isNil:
        if lastDot: precedingDot = true
        if parser.options.pathOperators:
          currentSymbol = ex
          precedingSymbol = currentSymbol
        else:
          result = ex
      elif lastDot:
        result = Expression(kind: Dot, left: result, right: ex)
      else:
        if parser.options.pathOperators:
          currentSymbol = ex
        else:
          finish()
    lastDot = false
    lastWhitespace = false

proc collectLineExpression*(exprs: sink seq[Expression]): Expression =
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
  var
    lineExprs: seq[Expression]
    comma: CommaKind
    waiting = false
    singleExprs: seq[Expression]
    indent: Expression
    indentIsDo: bool
  defer: # rather defer than put this at the end and try to put `break` everywhere
    if singleExprs.len != 0:
      lineExprs.add(collectLineExpression(move singleExprs))
    if not indent.isNil:
      if (parser.options.operatorIndentMakesBlock or indentIsDo) and
        lineExprs.len == 1 and lineExprs[0].kind in IndentableCallKinds:
        if parser.options.weirdOperatorIndentUnwrap and
          (let expandKinds =
            if indentIsDo: {Prefix, Infix, ExpressionKind.Colon}
            else: {Infix, ExpressionKind.Colon};
            lineExprs[0].kind in expandKinds):
          var ex = lineExprs[0]
          while ex.arguments[^1].kind in expandKinds:
            ex = ex.arguments[^1]
          if ex.arguments[^1].kind in IndentableCallKinds:
            ex.arguments[^1].arguments.add(indent)
          else:
            ex.arguments[^1] = Expression(kind: OpenCall,
              address: ex.arguments[^1], arguments: @[indent])
        elif parser.options.makeOperatorInfixOnIndent and
          lineExprs[0].kind in {Postfix, Prefix}:
          lineExprs[0] = Expression(kind: Infix,
            address: lineExprs[0].address,
            arguments: move(lineExprs[0].arguments) & indent)
        else:
          lineExprs[0].arguments.add(indent)
      else:
        lineExprs.add(indent)
    if lineExprs.len == 0:
      result = Expression(kind: ExpressionKind.None)
    elif comma == CommaList:
      result = Expression(kind: Comma, elements: lineExprs)
    elif lineExprs.len == 1:
      result = lineExprs[0]
    else:
      result = Expression(kind: OpenCall, address: lineExprs[0], arguments: lineExprs[1..^1])
  template finish = return
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace: discard
    of tkComma:
      if closed:
        # outside line scope
        dec parser.pos
        finish()
      waiting = true
      if singleExprs.len != 0: # consider ,, typo as ,
        let ex = collectLineExpression(move singleExprs)
        if not closed and comma == NoComma and ex.kind == OpenCall:
          assert ex.arguments.len == 1, "opencall with more than 1 argument should be impossible before comma"
          comma = CommaCall
          lineExprs.add(ex.address)
          lineExprs.add(ex.arguments)
        else:
          if comma == NoComma: comma = CommaList
          lineExprs.add(ex)
    of tkNewline:
      if waiting or closed:
        for tok in parser.nextTokens:
          if tok.kind notin {tkNone, tkWhitespace, tkNewline, tkIndent, tkIndentBack}:
            dec parser.pos
            break
      else:
        let originalPos = parser.pos
        for tok in parser.nextTokens:
          case tok.kind
          of tkNone, tkWhitespace, tkNewline: discard
          of tkIndent:
            if not waiting:
              inc parser.pos
              indent = parser.recordBlockLevel(indented = true)
              # current position will be immediately after indentback
              # and refer to token of new line 
              dec parser.pos
              finish()
          else:
            parser.pos = originalPos - 1 # ???
            finish()
      waiting = false
    of tkIndentBack:
      if not waiting:
        # outside line scope
        dec parser.pos
        finish()
    of tkSemicolon, tkCloseBrack, tkCloseCurly, tkCloseParen:
      # outside line scope
      dec parser.pos
      finish()
    of tkColon:
      waiting = true # should change this
      singleExprs.add(Expression(kind: Symbol, identifier: ":"))
    of tkIndent:
      if not waiting:
        inc parser.pos
        indent = parser.recordBlockLevel(indented = true)
        indentIsDo = false
        dec parser.pos # maybe should not be here
        finish()
    elif token.kind == tkBackslash and parser.options.backslashParenLine and
      parser.pos + 1 < parser.tokens.len and
      parser.tokens[parser.pos + 1].kind == tkOpenParen:
      inc parser.pos, 2
      singleExprs.add(parser.recordLineLevel(closed = false))
      inc parser.pos
      assert parser.tokens[parser.pos].kind == tkCloseParen, "wrong delimiter for backslash paren line"
    elif token.kind == tkBackslash and parser.options.backslashLine:
      inc parser.pos
      singleExprs.add(parser.recordLineLevel(closed))
      finish()
    elif token.kind == tkSymbol and token.raw == "do":
      waiting = false
      inc parser.pos # skip do
      while parser.tokens[parser.pos].kind in {tkNewline, tkWhitespace}:
        # skip newlines to not confuse line recorder
        inc parser.pos
      indent = parser.recordWideLine()
      indentIsDo = true
      finish()
    else:
      let ex = parser.recordSingle()
      waiting = false # symbols do not bypass this
      singleExprs.add(ex)
  finish()

proc parse*(tokens: sink seq[Token]): Expression =
  var parser = newParser(tokens)
  result = parser.recordBlockLevel()

when isMainModule:
  import tokenizer

  when false:
    let tests = [
      readFile("concepts/test.ba"),
      #readFile("concepts/tag.ba"),
      readFile("concepts/arguments.ba"),
      readFile("concepts/badspec.ba")
    ]

    for t in tests:
      echo "input: ", t
      echo "output: ", parse(tokenize(t))

  template tp(ss: string) =
    let s = ss
    let t = tokenize(s)
    echo t
    let p = parse(t)
    echo p
    echo p.repr

  when false:
    tp """
  a = \b
    c = \d
      e = \f
        g = \h
    i = \j
  k
  """
    tp """
  a do b do (c)
  d
  """
    tp """
do
  if a,
    b,
        c,
  d

  if a,
    do
      b
  \else c
"""
    tp """
try (do
  a
  b),
(catch c do
  d
  e),
(catch f do
  g),
(finally do
  e)
"""
    tp """
if (a, \
  b)
  c
"""
    tp "combination(n: Int, r: Int) = \\for result x = 1, each i in 0..<r do while i < r do x = x * (n - i) / (r - i)"
    tp "`for` a = b, c = d do e = f"
    tp "a := b + c * 4"
    tp "a:b:c + 1"
  else:
    tp "{:}"

  when false:
    import os

    for (k, p) in walkDir("concepts"):
      if k == pcFile:
        echo "file: ", p
        let e = parse(readFile(p))
        echo "expr: ", e
