import shortstring, expressions

type
  Precedence* = enum
    Access # dot operators
    Colon
    Exponent
    Shift
    Multiplication, Addition
    Range
    Conversion # as etc
    Comparison, Not, And, Or
    Misc
    Accusative # ? @ etc
    Separation # , maybe
    Assignment
    Lambda
    Statement # postfix if/for
    None
  
  Associativity* = enum Left, Right, Unary

const Associativities*: array[Precedence, Associativity] = [
  Access: Left,
  Colon: Right,
  Exponent: Right,
  Shift: Left,
  Multiplication: Left,
  Addition: Left,
  Range: Left,
  Conversion: Left,
  Comparison: Left,
  Not: Unary,
  And: Left,
  Or: Left,
  Misc: Left,
  Accusative: Right,
  Separation: Right,
  Assignment: Right,
  Lambda: Right,
  Statement: Left,
  None: Left
]

when shortStringIsArray:
  {.experimental: "caseStmtMacros".}

proc precedence*(symbol: ShortString): Precedence =
  var L: int
  case symbol
  of short"": None
  of short"=": Assignment
  of short":": Colon
  of short"**", short"pow": Exponent # not keyword
  of short"shl", short"shr": Shift # not keywords
  of short"div", short"mod", short"rem": Multiplication # rem not a keyword
  of short"as", short"from": Conversion
  of short"is", short"in", short"of": Comparison # maybe not of
  of short"not", short"!": Not
  of short"and", short"&&": And
  of short"or", short"||": Or
  of short"for", short"if", short"while", short"unless", short"until", short"do": Statement # not sure if these can be keywords
  elif (L = symbol.len; L > 1 and symbol[L - 2 .. L - 1] in [short"=>", short"->"]):
    Lambda
  else:
    case symbol[0]
    of '^': Exponent
    of '*', '%', '/', '&', '\\': Multiplication
    of '+', '-', '~', '|': Addition
    of '.':
      if L > 1 and symbol[1] == '.':
        Range
      else:
        Access
    of '<', '>', '!', '=': Comparison
    of '@', '?', ':': Accusative
    of ',', ';': Separation
    elif symbol[L - 1] == '=': Assignment
    else: Misc

proc reduceOperators*(exprs: sink seq[Expression], lowestKind = low(Precedence)): seq[Expression] =
  # use nils to delete expressions instead of reordering the seq
  if exprs.len <= 1: return exprs
  var prec = lowestKind
  var deleted = 0
  template delete(foo) =
    when false: # in case swap is bad
      if not foo.isNil:
        inc deleted
      foo = nil
    else:
      var old: Expression
      swap old, foo
      if not old.isNil:
        inc deleted
  while prec != Precedence.None:
    proc isOperator(e: Expression, precedence = prec): bool {.inline, nimcall.} =
      # xxx maybe have a precedence field in symbol
      e.kind == Symbol and e.symbol.precedence == precedence
    let assoc = Associativities[prec]
    var mustPrefix = true
    var prefixStack: seq[Expression]
    var i = 0
    while i < exprs.len:
      var e = exprs[i]
      if e.isNil: discard
      elif e.isOperator:
        if mustPrefix:
          prefixStack.add(e)
        mustPrefix = true
      else:
        let psl = prefixStack.len
        while prefixStack.len != 0:
          e = Expression(kind: Prefix, address: prefixStack.pop, arguments: @[e]).inferInfo()
        for j in i - psl ..< i:
          delete exprs[j]
        exprs[i] = e
        mustPrefix = false
      inc i
    if mustPrefix:
      let startIndex = max(0, exprs.len - prefixStack.len - 2)
      var e = exprs[startIndex]
      for i in startIndex + 1 ..< exprs.len:
        e = Expression(kind: Postfix, address: exprs[i], arguments: @[e]).inferInfo()
        delete exprs[i]
      exprs[startIndex] = e
    case assoc
    of Left:
      var lhsStart, i = 0
      var lhs, op: Expression
      while i < exprs.len:
        let e = exprs[i]
        if e.isNil: discard
        elif e.isOperator:
          op = e
        elif op.isNil:
          lhs = e
          lhsStart = i
        else:
          lhs = makeInfix(op, lhs, e)
          for j in lhsStart ..< i:
            delete exprs[j]
          exprs[i] = lhs
          discard lhs # arc destroys lhs here otherwise
          op = nil
        inc i
    of Right:
      var rhsStart, i = exprs.high
      var rhs, op: Expression
      while i >= 0:
        let e = exprs[i]
        if e.isNil: discard
        elif e.isOperator:
          op = e
        elif op.isNil:
          rhs = e
          rhsStart = i
        else:
          rhs = makeInfix(op, e, rhs)
          for j in i ..< rhsStart:
            delete exprs[j]
          exprs[rhsStart] = rhs
          discard rhs # arc deletes rhs otherwise
          op = nil
        dec i
    of Unary:
      var stack: seq[Expression]
      var i = 0
      while i < exprs.len:
        var e = exprs[i]
        if e.isNil: discard
        elif e.isOperator:
          stack.add(e)
        else:
          let sl = stack.len
          while stack.len != 0:
            e = Expression(kind: Prefix, address: stack.pop, arguments: @[e]).inferInfo()
          for j in i - sl ..< i:
            delete exprs[j]
          exprs[i] = e
        inc i
    inc prec
  result = newSeqOfCap[Expression](exprs.len - deleted)
  for e in exprs:
    if not e.isNil:
      result.add(e)
