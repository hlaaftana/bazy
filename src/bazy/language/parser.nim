import shortstring, tokens, expressions, operators

#[

notes:

* probably (not) a lot of indent bugs when you indent in parentheses (delegated to tokenizer) or with variable spaces
* some kind of delimiter counter/stack on parser object would stop infinite loops from errors
* keep track of indents with a counter (maybe not)
* maybe make distinction between indent and normal argument, or `do` and indent, no idea
* maybe operators like <| or god versions like <-| accept commands with commas on left hand side
  (commands-after-operators already works on the right hand side but without commas)
* maybe make commas and semicolons operators, this would depend on
  command-after-operator transformation for `a b, c d`
  these would not be infixes, they would be one list of expressions
* maybe command-after-operator also works with commas

]#

type
  ParserOptions* = object
    curlyBlocks*,
      pathOperators*,
      operatorIndentMakesBlock*,
      backslashLine*,
      backslashPostArgument*,
      colonPostArgument*,
      weirdOperatorIndentUnwrap*,
      makeOperatorInfixOnIndent*,
      backslashParenLine*
    : bool
    postArgumentColonKeywords*: seq[ShortString] # performance hazard

  Parser* = ref object
    tokens*: seq[Token]
    pos*: int
    options*: ParserOptions

proc defaultOptions*(): ParserOptions =
  result = ParserOptions(
    curlyBlocks: false,
    pathOperators: true,
    operatorIndentMakesBlock: true,
    backslashLine: false,#true, # should be false
    backslashPostArgument: true,
    colonPostArgument: false,
    weirdOperatorIndentUnwrap: true,
    makeOperatorInfixOnIndent: true,
    backslashParenLine: true)
  result.postArgumentColonKeywords.add(short"else")

proc newParser*(tokens: sink seq[Token] = @[], options = defaultOptions()): Parser {.inline.} =
  Parser(tokens: tokens, options: options)

iterator nextTokens*(p: var Parser): Token =
  while p.pos < p.tokens.len:
    let t = p.tokens[p.pos]
    yield t
    inc p.pos
  
proc info*(p: var Parser): TokenInfo {.inline.} =
  p.tokens[p.pos].info

template conservePosNextIteration(p: var Parser) =
  dec p.pos

proc makeStringExpression*(s: sink string): Expression {.inline.} =
  Expression(kind: String, str: unescape(s))

proc recordLineLevel*(parser: var Parser, info: TokenInfo, closed = false): Expression

proc recordWideLine*(parser: var Parser, info: TokenInfo, closed = false): Expression =
  var s: seq[Expression]
  var semicoloned = false
  for t in parser.nextTokens:
    case t.kind
    of tkSemicolon:
      semicoloned = true
    of tkComma, tkCloseParen, tkCloseBrack, tkCloseCurly,
      tkIndentBack, tkNewline: # newline could be delegated to line
      break
    elif s.len == 0 or semicoloned:
      s.add(parser.recordLineLevel(t.info, closed))
      parser.conservePosNextIteration()
    else:
      break
  if semicoloned: Expression(kind: SemicolonBlock, statements: s, info: info)
  elif s.len == 0: Expression(kind: ExpressionKind.None, info: info)
  else: s[0]

proc recordBlockLevel*(parser: var Parser, indented = false): Expression =
  result = Expression(kind: Block, info: parser.info)
  defer:
    if result.statements.len == 1: result = result.statements[0]
  var backslashNames: seq[string]
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace, tkNewline: continue
    of tkSemicolon: discard
    of tkIndentBack:
      if indented:
        inc parser.pos
        break
      else:
        echo "warning extra indentback at " & $token.info
        continue
    of tkComma, tkCloseBrack, tkCloseCurly, tkCloseParen:
      assert indented, "extra delim " & $token.kind
      break
    elif token.kind == tkColon and 
      parser.options.colonPostArgument and
      result.statements.len > 0 and
      result.statements[^1].kind in IndentableCallKinds:
      inc parser.pos
      result.statements[^1].arguments.add(parser.recordWideLine(parser.info))
      parser.conservePosNextIteration()
    else:
      result.statements.add(parser.recordWideLine(token.info))
      parser.conservePosNextIteration()
    reset backslashNames

proc recordBrack*(parser: var Parser, info: TokenInfo): Expression =
  var s: seq[Expression]
  var ended = false
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline, tkComma: discard
    of tkCloseBrack:
      inc parser.pos
      ended = true
      break
    of tkCloseCurly, tkCloseParen:
      assert false, "wrong delim for brack"
    else:
      s.add(parser.recordWideLine(t.info, closed = true))
      parser.conservePosNextIteration()
  assert ended, "missing ending brack"
  Expression(kind: Array, elements: s, info: info)

proc recordParen*(parser: var Parser, info: TokenInfo): Expression =
  var commad = false
  var s: seq[Expression]
  var ended = false
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline: discard
    of tkComma: commad = true
    of tkCloseParen:
      inc parser.pos
      ended = true
      break
    of tkCloseCurly, tkCloseBrack:
      assert false, "wrong delim for paren"
    elif s.len == 0 or commad:
      s.add(parser.recordWideLine(t.info, closed = true))
      parser.conservePosNextIteration()
    else:
      break
  assert ended, "missing ending paren"
  if not commad and s.len == 1: Expression(kind: Wrapped, wrapped: s[0], info: info)
  #elif not commad and s.len == 0: Expression(kind: ExpressionKind.None)
  else: Expression(kind: Tuple, elements: s, info: info)

proc recordCurly*(parser: var Parser, info: TokenInfo): Expression =
  var s: seq[Expression]
  var commad = false
  var ended = false
  for t in parser.nextTokens:
    case t.kind
    of tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewline: discard
    of tkComma: commad = true
    of tkCloseCurly:
      inc parser.pos
      ended = true
      break
    of tkCloseBrack, tkCloseParen:
      assert false, "wrong delim for curly"
    elif s.len == 0 or commad or parser.options.curlyBlocks:
      s.add(parser.recordWideLine(t.info, closed = not parser.options.curlyBlocks or commad))
      parser.conservePosNextIteration()
  assert ended, "missing ending curly"
  if commad: Expression(kind: Set, elements: s, info: info)
  else: Expression(kind: Block, statements: s, info: info)

proc recordSingle*(parser: var Parser, info: TokenInfo): Expression =
  # +a is a path
  # a.b[c] is a path
  # a+b/c is a path and implies (a + b) / c
  var
    precedingSymbol, currentSymbol: Expression = nil
    precedingDot, lastWhitespace, lastDot = false
  defer:
    # paths will never terminate with delimiter
    let resultWasNil = result.isNil
    if not currentSymbol.isNil:
      if not parser.options.pathOperators:
        assert false, "currentSymbol should not exist when path operators are off"
      if resultWasNil:
        result = currentSymbol
      else:
        result = Expression(kind: PathPostfix, address: currentSymbol, arguments: @[result], info: info)
    if not precedingSymbol.isNil and not resultWasNil:
      if not parser.options.pathOperators:
        assert false, "precedingSymbol should not exist when path operators are off"
      result = Expression(kind: PathPrefix, address: precedingSymbol, arguments: @[result], info: info)
    if precedingDot:
      result = Expression(kind: PathPrefix,
        address: Expression(kind: Symbol, symbol: short"."),
        arguments: @[result], info: info)
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
            Expression(kind: String, str: unescape(token.content), info: token.info)
          else:
            Expression(kind: String, str: token.content, info: token.info)
        of tkNumber: Expression(kind: Number, number: token.num, info: token.info)
        of tkWord: Expression(kind: Name, identifier: token.raw, info: token.info)
        else: nil
      if result.isNil:
        result = ex
        if lastDot:
          precedingDot = true
        if not currentSymbol.isNil:
          precedingSymbol = currentSymbol
          currentSymbol = nil
      elif lastDot:
        result = Expression(kind: Dot, left: result, right: ex, info: result.info)
      elif not currentSymbol.isNil:
        result = Expression(kind: PathInfix, address: currentSymbol, arguments: @[result, ex], info: result.info)
        currentSymbol = nil
      else:
        result = Expression(kind: PathPrefix, address: result, arguments: @[ex], info: result.info)
    of tkOpenBrack, tkOpenCurly, tkOpenParen:
      if lastWhitespace and not lastDot:
        finish()
      inc parser.pos
      let inf = token.info
      let ex = case token.kind
        of tkOpenBrack: parser.recordBrack(inf)
        of tkOpenParen: parser.recordParen(inf)
        of tkOpenCurly: parser.recordCurly(inf)
        else: nil
      if result.isNil:
        result = ex
        if lastDot: precedingDot = true
      elif not currentSymbol.isNil:
        result = Expression(kind: PathInfix, address: currentSymbol, arguments: @[result, ex], info: inf)
        currentSymbol = nil
      else:
        result = (case ex.kind
        of Tuple:
          if result.kind == Dot and not lastDot:
            Expression(kind: PathCall, address: result.right, arguments: @[result.left] & ex.elements)
          else:
            Expression(kind: PathCall, address: result, arguments: ex.elements)
        of Array: Expression(kind: Subscript, address: result, arguments: ex.elements)
        of Set: Expression(kind: CurlySubscript, address: result, arguments: ex.elements)
        of Wrapped: # single expression
          if result.kind == Dot and not lastDot:
            Expression(kind: PathCall, address: result.right, arguments: @[result.left, ex.wrapped])
          else:
            Expression(kind: PathCall, address: result, arguments: @[ex.wrapped])
        of ExpressionKind.None:
          Expression(kind: PathCall, address: result, arguments: @[])
        else:
          assert false, "should return one of tuple, array, set, wrapped"
          nil).withInfo(token.info)
      parser.conservePosNextIteration()
    of tkDot:
      lastDot = true
      lastWhitespace = false
      continue
    of tkSymbol, tkBackslash:
      if lastWhitespace and not lastDot:
        finish()
      let ex = Expression(kind: Symbol, symbol:
        if token.kind == tkSymbol: token.short
        else: short"\\", info: token.info)
      if result.isNil:
        if lastDot: precedingDot = true
        if parser.options.pathOperators:
          currentSymbol = ex
          precedingSymbol = currentSymbol
        else:
          result = ex
      elif lastDot:
        result = Expression(kind: Dot, left: result, right: ex, info: token.info)
      else:
        if parser.options.pathOperators:
          currentSymbol = ex
        else:
          finish()
    lastDot = false
    lastWhitespace = false

proc collectLineExpression*(exprs: sink seq[Expression], info: TokenInfo): Expression =
  if exprs.len == 0: return Expression(kind: ExpressionKind.None, info: info)
  var terms = reduceOperators(exprs)
  result = terms.pop
  while terms.len != 0:
    let callee = terms.pop
    if callee.kind in {Prefix, Infix, ExpressionKind.Colon}: # postfix should not be possible
      # command-after-operator support
      var deepest = callee
      while deepest.arguments[^1].kind in {Prefix, Infix, ExpressionKind.Colon}:
        deepest = deepest.arguments[^1]
      deepest.arguments[^1] = Expression(kind: OpenCall, address: deepest.arguments[^1], arguments: @[result]).inferInfo()
      result = callee
    else:
      result = Expression(kind: OpenCall, address: callee, arguments: @[result], info: callee.info)

proc recordLineLevel*(parser: var Parser, info: TokenInfo, closed = false): Expression =
  type CommaKind = enum
    NoComma, CommaCall, CommaList
  var
    lineExprs: seq[Expression]
    comma: CommaKind
    waiting = false
    singleExprs: seq[Expression]
    indent: Expression
    indentIsDo: bool
    postArg: Expression
  defer: # rather defer than put this at the end and try to put `break` everywhere
    if singleExprs.len != 0:
      lineExprs.add(collectLineExpression(singleExprs, info))
      reset(singleExprs)
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
              address: ex.arguments[^1], arguments: @[indent]).inferInfo()
        elif parser.options.makeOperatorInfixOnIndent and
          lineExprs[0].kind in {Postfix, Prefix}:
          if lineExprs[0].arguments.len == 1:
            lineExprs[0] = makeInfix(lineExprs[0].address, lineExprs[0].arguments[0], indent)
          else:
            lineExprs[0] = Expression(kind: Infix,
              address: lineExprs[0].address,
              arguments: lineExprs[0].arguments & indent).inferInfo()
        else:
          lineExprs[0].arguments.add(indent)
      else:
        lineExprs.add(indent)
    if not postArg.isNil:
      if lineExprs[0].kind in IndentableCallKinds:
        lineExprs[0].arguments.add(postArg)
      else:
        lineExprs.add(postArg)
    if lineExprs.len == 0:
      result = Expression(kind: ExpressionKind.None, info: info)
    elif comma == CommaList:
      result = Expression(kind: Comma, elements: lineExprs, info: info)
    elif lineExprs.len == 1:
      result = lineExprs[0]
    else:
      result = Expression(kind: OpenCall, address: lineExprs[0], arguments: lineExprs[1..^1], info: info)
  template finish = return
  for token in parser.nextTokens:
    case token.kind
    of tkNone, tkWhitespace: discard
    of tkComma:
      if closed:
        # outside line scope
        finish()
      waiting = true
      if singleExprs.len != 0: # consider ,, typo as ,
        let ex = collectLineExpression(singleExprs, info)
        reset(singleExprs)
        if not closed and comma == NoComma and ex.kind == OpenCall:
          assert ex.arguments.len == 1, "opencall with more than 1 argument should be impossible before comma"
          comma = CommaCall
          lineExprs.add(ex.address)
          lineExprs.add(ex.arguments)
        else:
          if comma == NoComma: comma = CommaList
          lineExprs.add(ex)
    of tkNewline, tkIndent:
      if waiting or (token.kind == tkNewline and closed):
        if token.kind == tkNewline:
          for tok in parser.nextTokens:
            if tok.kind notin {tkNone, tkWhitespace, tkNewline, tkIndent, tkIndentBack}:
              parser.conservePosNextIteration()
              break
      else:
        var lastPos = parser.pos
        var indentRecorded = false
        for tok in parser.nextTokens:
          case tok.kind
          of tkNone, tkWhitespace, tkNewline: discard
          elif tok.kind == tkIndent and not indentRecorded:
            inc parser.pos
            indent = parser.recordBlockLevel(indented = true)
            indentRecorded = true
            lastPos = parser.pos
            parser.conservePosNextIteration()
          elif (parser.options.backslashPostArgument and tok.kind == tkBackslash) or
            (parser.options.postArgumentColonKeywords.len != 0 and tok.kind == tkSymbol and
              tok.short in parser.options.postArgumentColonKeywords):
            let name =
              if tok.kind == tkSymbol:
                Expression(kind: Name, identifier: $tok.short, info: tok.info)
              else:
                nil
            inc parser.pos
            for tok in parser.nextTokens:
              case tok.kind
              of tkNone, tkWhitespace, tkNewline: discard
              else: break
            let value = parser.recordLineLevel(tok.info, closed) # closed is clearly false here
            if name.isNil:
              postArg = value
            else:
              postArg = Expression(kind: ExpressionKind.Colon, left: name, right: value, info: tok.info)
            lastPos = parser.pos
            parser.conservePosNextIteration()
          else:
            break
        parser.pos = lastPos
        finish()
      waiting = false
    of tkIndentBack:
      if not waiting:
        # outside line scope
        finish()
    of tkSemicolon, tkCloseBrack, tkCloseCurly, tkCloseParen:
      # outside line scope
      finish()
    of tkColon:
      singleExprs.add(Expression(kind: Symbol, symbol: short":", info: token.info))
    elif token.kind == tkBackslash and parser.options.backslashParenLine and
      parser.pos + 1 < parser.tokens.len and
      parser.tokens[parser.pos + 1].kind == tkOpenParen:
      inc parser.pos, 2
      singleExprs.add(parser.recordLineLevel(token.info, closed = false))
      assert parser.tokens[parser.pos].kind == tkCloseParen, "wrong delimiter for backslash paren line"
      inc parser.pos
    elif token.kind == tkBackslash and parser.options.backslashLine:
      inc parser.pos
      singleExprs.add(parser.recordLineLevel(token.info, closed))
      finish()
    elif token.kind == tkSymbol and token.short == short"do":
      waiting = false
      inc parser.pos # skip do
      while parser.tokens[parser.pos].kind in {tkNewline, tkWhitespace}:
        # skip newlines to not confuse line recorder
        inc parser.pos
      indent = parser.recordWideLine(token.info)
      indentIsDo = true
      finish()
    else:
      let ex = parser.recordSingle(token.info)
      waiting = false # symbols do not bypass this
      singleExprs.add(ex)
      parser.conservePosNextIteration()
  finish()

proc parse*(tokens: sink seq[Token]): Expression =
  var parser = newParser(tokens)
  result = copy parser.recordBlockLevel()

when isMainModule:
  import tokenizer

  #echo parse(tokenize(readFile("concepts/test.ba")))

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
  else c
"""
    tp "combination(n: Int, r: Int) = \\for result x = 1, each i in 0..<r do while i < r do x = x * (n - i) / (r - i)"
    tp "`for` a = b, c = d do e = f"
    tp "a := b + c * 4"
    tp "a:b:c + 1"
    tp "{:}"
    tp "a + b - c"
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
    tp """
if (a
  b)
  c
"""
    tp """
if (a
    b
      c)
  c
"""
    tp """
a
  b
    c
d
"""
    tp """
if a
  if b
    c
  \else
    d
\else
  e
"""
    tp """
if a
  if b
    c
    c2
  \else:
    d
    d2
\else:
  e
  f
"""
    tp """
if a
  if b
    c
    c2
  else
    d
    d2
else
  e
  f
"""
  else:
    tp """
try do
  a
  b; ,
catch c do
  d
  e; ,
catch f do
  g; ,
finally do
  e
"""
    discard

  when false:
    import os

    for (k, p) in walkDir("concepts"):
      if k == pcFile:
        echo "file: ", p
        let e = parse(readFile(p))
        echo "expr: ", e
