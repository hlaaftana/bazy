import tokenizer

type
  ExpressionKind* = enum
    None
    Number, String
    Name, Symbol
    Call, Infix, Prefix, Postfix, Subscript, CurlySubscript
    Dot, Colon
    Tuple, Array, Set
    Block
  Expression* {.acyclic.} = ref object
    case kind*: ExpressionKind
    of None: discard
    of Number:
      number*: NumberToken
    of String:
      str*: string
    of Name, Symbol:
      identifier*: string
    of Call, Infix, Prefix, Postfix, Subscript, CurlySubscript:
      address*: Expression
      arguments*: seq[Expression]
    of Dot, Colon:
      left*, right*: Expression
    of Tuple, Array, Set:
      # these can be colon expressions
      elements*: seq[Expression]
    of Block:
      statements*: seq[Expression]

from strutils import join

proc `$`*(ex: Expression): string =
  if ex.isNil: return "nil"
  case ex.kind
  of None: "()"
  of Number: $ex.number
  of String: "\"" & ex.str & "\""
  of Name, Symbol: ex.identifier
  of Call: $ex.address & "(" & ex.arguments.join(", ") & ")"
  of Infix: "(" & $ex.arguments[0] & " " & $ex.address & " " & $ex.arguments[1] & ")"
  of Postfix: "(" & $ex.arguments[0] & " " & $ex.address & ")"
  of Prefix: "(" & $ex.address & " " & $ex.arguments[0] & ")"
  of Subscript: $ex.address & "[" & ex.arguments.join(", ") & "]"
  of CurlySubscript: $ex.address & "{" & ex.arguments.join(", ") & "}"
  of Dot: $ex.left & "." & $ex.right
  of Colon: $ex.left & ": " & $ex.right
  of Tuple: "(" & ex.elements.join(", ") & ")"
  of Array: "[" & ex.elements.join(", ") & "]"
  of Set: "{" & ex.elements.join(", ") & "}"
  of Block: "(" & ex.statements.join("; ") & ")"
