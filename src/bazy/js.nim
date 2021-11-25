# not real module
import expressions, tokenizer
from strutils import toHex

proc mangle*(s: string): string =
  proc validJsName(name: string): bool =
    result = true
    const reservedWords = ["abstract", "await", "boolean", "break", "byte",
      "case", "catch", "char", "class", "const", "continue", "debugger",
      "default", "delete", "do", "double", "else", "enum", "export", "extends",
      "false", "final", "finally", "float", "for", "function", "goto", "if",
      "implements", "import", "in", "instanceof", "int", "interface", "let",
      "long", "native", "new", "null", "package", "private", "protected",
      "public", "return", "short", "static", "super", "switch", "synchronized",
      "this", "throw", "throws", "transient", "true", "try", "typeof", "var",
      "void", "volatile", "while", "with", "yield"]
    case name
    of reservedWords:
      return false
    else:
      discard
    if name[0] in {'0'..'9'}: return false
    for chr in name:
      if chr notin {'A'..'Z','a'..'z','_','$','0'..'9'}:
        return false
  if s.validJsName:
    result = s
  else:
    result = newStringOfCap(s.len)
    for c in s:
      case c
      of 'A'..'Z', 'a'..'z', '_', '0'..'9':
        result.add c
      else:
        result.add("HEX" & toHex(ord(c), 2))

proc toJs*(ex: Expression): string =
  case ex.kind
  of None: result = ""
  of Number: result = $ex.number
  of String:
    for c in ex.str:
      result.addEscapedChar(c)
  of Name, Symbol: result = mangle(ex.identifier)
  of Wrapped: result = "(" & toJs(ex.wrapped) & ")"
  of OpenCall, WrappedCall, Infix, Prefix, Postfix:
    discard
  of Subscript, CurlySubscript:
    result = "(" & toJs(ex.address) & ")[" & ex.arguments[0].toJs & "]"
  of Dot: result = "(" & toJs(ex.left) & ")." & toJs(ex.right)
  of Colon: result = toJs(ex.left) & ": " & toJs(ex.right)
  of Tuple, Array:
    result = "["
    for e in ex.elements:
      if result.len > 1:
        result.add(", ")
      result.add(e.toJs)
    result.add("]")
  of Set:
    result = "new Set(["
    for e in ex.elements:
      if result.len > "new Set([".len:
        result.add(", ")
      result.add(e.toJs)
    result.add("])")
  of Block, SemicolonBlock:
    for e in ex.statements:
      result.add(e.toJs)
      result.add(";\n")