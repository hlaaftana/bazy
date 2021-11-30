import tokenizer

type
  ExpressionKind* = enum
    None
    Number, String
    Name, Symbol
    Wrapped
    OpenCall, Infix, Prefix, Postfix
    PathCall, PathInfix, PathPrefix, PathPostfix
    Subscript, CurlySubscript
    Dot, Colon
    Tuple, Array, Set
    Block, SemicolonBlock
  Expression* {.acyclic.} = ref object
    case kind*: ExpressionKind
    of None: discard
    of Number:
      number*: NumberToken
    of String:
      str*: string
    of Name, Symbol:
      identifier*: string
    of Wrapped:
      wrapped*: Expression
    of OpenCall, Infix, Prefix, Postfix,
      PathCall, PathInfix, PathPrefix, PathPostfix,
      Subscript, CurlySubscript:
      address*: Expression
      arguments*: seq[Expression]
    of Dot, Colon:
      left*, right*: Expression
    of Tuple, Array, Set:
      # these can be colon expressions
      elements*: seq[Expression]
    of Block, SemicolonBlock:
      statements*: seq[Expression]

proc `==`*(a, b: Expression): bool =
  if a.isNil and b.isNil: return true
  if (a.isNil xor b.isNil) or (a.kind != b.kind): return false
  result = case a.kind
  of None: true
  of Number: a.number == b.number
  of String: a.str == b.str
  of Name, Symbol: a.identifier == b.identifier
  of Wrapped: a.wrapped == b.wrapped
  of OpenCall, Infix, Prefix, Postfix,
    PathCall, PathInfix, PathPrefix, PathPostfix,
    Subscript, CurlySubscript:
    a.address == b.address and a.arguments == b.arguments
  of Dot, Colon: a.left == b.left and a.right == b.right
  of Tuple, Array, Set: a.elements == b.elements
  of Block, SemicolonBlock: a.statements == b.statements

const
  OpenCallKinds* = {OpenCall, Infix, Prefix, Postfix}
  PathCallKinds* = {PathCall, PathInfix, PathPrefix, PathPostfix}
  CallKinds* = OpenCallKinds + PathCallKinds
  IndentableCallKinds* = OpenCallKinds + {PathCall}

proc makeInfix*(op, a, b: Expression): Expression =
  if op.kind == Symbol and op.identifier == ":":
    Expression(kind: Colon, left: a, right: b)
  else:
    Expression(kind: Infix, address: op, arguments: @[a, b])

import strutils

proc `$`*(ex: Expression): string =
  if ex.isNil: return "nil"
  case ex.kind
  of None: "()"
  of Number: $ex.number
  of String: "\"" & ex.str & "\""
  of Name, Symbol: ex.identifier
  of Wrapped: "(" & $ex.wrapped & ")"
  of OpenCall: "(" & $ex.address & " " & ex.arguments.join(", ") & ")"
  of Infix:
    "(" & $ex.arguments[0] & " " & $ex.address & " " & $ex.arguments[1] & ")" &
      (if ex.arguments.len > 2: " {" & ex.arguments[2..^1].join(", ") & "}" else: "")
  of Postfix:
    "(" & $ex.arguments[0] & " " & $ex.address & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of Prefix:
    "(" & $ex.address & " " & $ex.arguments[0] & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of PathCall: $ex.address & "(" & ex.arguments.join(", ") & ")"
  of PathInfix:
    "(" & $ex.arguments[0] & $ex.address & $ex.arguments[1] & ")" &
      (if ex.arguments.len > 2: " {" & ex.arguments[2..^1].join(", ") & "}" else: "")
  of PathPostfix:
    "(" & $ex.arguments[0] & $ex.address & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of PathPrefix:
    "(" & $ex.address & $ex.arguments[0] & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of Subscript: $ex.address & "[" & ex.arguments.join(", ") & "]"
  of CurlySubscript: $ex.address & "{" & ex.arguments.join(", ") & "}"
  of Dot: $ex.left & "." & $ex.right
  of Colon: $ex.left & ": " & $ex.right
  of Tuple: "(" & ex.elements.join(", ") & ")"
  of Array: "[" & ex.elements.join(", ") & "]"
  of Set: "{" & ex.elements.join(", ") & "}"
  of Block:
    var s = "(\n"
    for i in 0 ..< ex.statements.len:
      let ss = $ex.statements[i]
      for sl in ss.splitLines:
        s.add("  " & sl & "\n")
      if i < ex.statements.len - 1:
        s[^1 .. ^1] = ";\n"
    s.add(")")
    move s
  of SemicolonBlock: "(" & ex.statements.join("; ") & ")"

import unicode

proc unescape*(s: string): string =
  var
    escaped = false
    uBase: int
    uNum = 0
    startedU = -1
  result = newStringOfCap(s.len)
  var i = 0
  while i < s.len:
    let c = s[i]
    if startedU != -1:
      case c
      of '_': discard
      # could change these to revert if the base is wrong
      of '0'..'9': uNum = uNum * uBase + int(c.byte - '0'.byte)
      of 'a'..'f': uNum = uNum * uBase + 10 + int(c.byte - 'a'.byte)
      of 'A'..'F': uNum = uNum * uBase + 10 + int(c.byte - 'A'.byte)
      else:
        result.add(if c == '}': $Rune(uNum) else: s[startedU..i])
        uNum = 0
        startedU = -1
    elif escaped:
      if c in {'u', 'U'}:
        if i + 1 < s.len and s[i + 1] == '{':
          uBase = 16
          startedU = i - 1
        elif i + 2 < s.len and s[i + 1] in {'x', 'o', 'd', 'b',
          'X', 'O', 'D', 'B'} and s[i + 2] == '{':
          uBase = case s[i + 1]
          of 'x', 'X': 16
          of 'o', 'O': 8
          of 'd', 'D': 10
          of 'b', 'B': 2
          else: -1 # unreachable
          startedU = i - 1
        elif i + 4 < s.len and {s[i + 1], s[i + 2],
          s[i + 3], s[i + 4]} <= HexDigits:
          # can change parseHexInt to something more optimized but doesnt matter
          result.add($Rune(parseHexInt(s[i + 1 .. i + 4])))
        else:
          result.add('\\')
          result.add(c)
      elif c in {'x', 'X'} and {s[i + 1], s[i + 2]} <= HexDigits:
        result.add(char(parseHexInt(s[i + 1 .. i + 2])))
      else:
        let ch = case c
        of 't': '\t'
        of '"': '"'
        of '\'': '\''
        of '`': '`'
        of '\\': '\\'
        of 'r': '\r'
        of 'n': '\n'
        of 'f': '\f'
        of 'v': '\v'
        of 'a': '\a'
        of 'b': '\b'
        of 'e': '\e'
        else:
          result.add('\\')
          c
        result.add(ch)
      escaped = false
    elif c == '\\': escaped = true
    else: result.add(c)
    inc i
