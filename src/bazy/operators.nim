import expressions

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
    Separation # julia uses this for => pairings, no idea what to use for here
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

proc precedence*(symbol: string): Precedence =
  if symbol.len == 0: return None
  case symbol
  of "": None
  of "=": Assignment
  of ":": Colon
  of "**", "pow": Exponent # not keyword
  of "shl", "shr": Shift # not keywords
  of "div", "mod", "rem": Multiplication # rem not a keyword
  of "as", "from": Conversion
  of "is", "in", "of": Comparison # maybe not of
  of "not", "!": Not
  of "and", "&&": And
  of "or", "||": Or
  of "for", "if", "while", "unless", "until", "do": Statement # not sure if these can be keywords
  elif symbol.len > 1 and symbol[^2 .. ^1] in ["=>", "->"]:
    Lambda
  else:
    case symbol[0]
    of '^': Exponent
    of '*', '%', '/', '&', '\\': Multiplication
    of '+', '-', '~', '|': Addition
    of '.':
      if symbol.len > 1 and symbol[1] == '.':
        Range
      else:
        Access
    of '<', '>', '!', '=': Comparison
    of '@', '?', ':': Accusative
    elif symbol[^1] == '=': Assignment
    else: Misc

proc reduceOperators*(exprs: seq[Expression], lowestKind = low(Precedence)): seq[Expression] =
  # linked list should improve performance
  result = exprs
  if exprs.len <= 1: return
  var prec = lowestKind
  while prec != Precedence.None:
    template isOperator(e: Expression): bool = e.kind == Symbol and e.identifier.precedence == prec
    let assoc = Associativities[prec]
    var mustPrefix = true
    var prefixStack: seq[Expression]
    var i = 0
    while i < result.len:
      var e = result[i]
      if e.isOperator:
        if mustPrefix:
          prefixStack.add(e)
        mustPrefix = true
      else:
        let psl = prefixStack.len
        while prefixStack.len != 0:
          e = Expression(kind: Prefix, address: prefixStack.pop, arguments: @[e])
        result[i - psl .. i] = [e]
        i -= psl
        mustPrefix = false
      inc i
    if mustPrefix:
      let startIndex = if prefixStack.len + 1 == result.len: 0 else: result.len - prefixStack.len - 2
      var e = result[startIndex]
      for i in startIndex + 1 ..< result.len:
        e = Expression(kind: Postfix, address: result[i], arguments: @[e])
      result[startIndex .. ^1] = [e]
    case assoc
    of Left:
      var lhsStart, i = 0
      var lhs, op: Expression
      while i < result.len:
        var e = result[i]
        if e.isOperator:
          op = e
        elif op.isNil:
          lhs = e
          lhsStart = i
        else:
          lhs = makeInfix(op, lhs, e)
          result[lhsStart .. i] = [lhs]
          i = lhsStart
          op = nil
        inc i
    of Right:
      var rhsStart, i = result.high
      var rhs, op: Expression
      while i >= 0:
        var e = result[i]
        if e.isOperator:
          op = e
        elif op.isNil:
          rhs = e
          rhsStart = i
        else:
          rhs = makeInfix(op, e, rhs)
          result[i .. rhsStart] = [rhs]
          i = rhsStart - 1
          op = nil
        dec i
    of Unary:
      var stack: seq[Expression]
      var i = 0
      while i < result.len:
        var e = result[i]
        if e.isOperator:
          stack.add(e)
        else:
          let sl = stack.len
          while stack.len != 0:
            e = Expression(kind: Prefix, address: stack.pop, arguments: @[e])
          result[i - sl .. i] = [e]
          i -= sl
        inc i
    inc prec
