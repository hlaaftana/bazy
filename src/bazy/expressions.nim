import tokenizer

type
  ExpressionKind* = enum
    None
    Number, String
    Name, Symbol
    Wrapped
    OpenCall, WrappedCall, Infix, Prefix, Postfix
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
    of OpenCall, WrappedCall, Infix, Prefix, Postfix, Subscript, CurlySubscript:
      address*: Expression
      arguments*: seq[Expression]
    of Dot, Colon:
      left*, right*: Expression
    of Tuple, Array, Set:
      # these can be colon expressions
      elements*: seq[Expression]
    of Block, SemicolonBlock:
      statements*: seq[Expression]

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
  of WrappedCall: $ex.address & "(" & ex.arguments.join(", ") & ")"
  of Infix:
    "(" & $ex.arguments[0] & " " & $ex.address & " " & $ex.arguments[1] & ")" &
      (if ex.arguments.len > 2: " {" & ex.arguments[2..^1].join(", ") & "}" else: "")
  of Postfix:
    "(" & $ex.arguments[0] & " " & $ex.address & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of Prefix:
    "(" & $ex.address & " " & $ex.arguments[0] & ")" &
      (if ex.arguments.len > 1: " {" & ex.arguments[1..^1].join(", ") & "}" else: "")
  of Subscript: $ex.address & "[" & ex.arguments.join(", ") & "]"
  of CurlySubscript: $ex.address & "{" & ex.arguments.join(", ") & "}"
  of Dot: $ex.left & "." & $ex.right
  of Colon: $ex.left & ": " & $ex.right
  of Tuple: "(" & ex.elements.join(", ") & ")"
  of Array: "[" & ex.elements.join(", ") & "]"
  of Set: "{" & ex.elements.join(", ") & "}"
  of Block, SemicolonBlock:
    var s = "(\n"
    for i in 0 ..< ex.statements.len:
      let ss = $ex.statements[i]
      for sl in ss.splitLines:
        s.add("  " & sl & "\n")
      if i < ex.statements.len - 1:
        s[^1 .. ^1] = ";\n"
    s & ")"
