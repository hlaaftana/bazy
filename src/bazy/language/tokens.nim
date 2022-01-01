import number, ../defines

type
  TokenKind* = enum
    tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewLine
    tkBackslash, tkDot, tkComma, tkColon, tkSemicolon
    tkOpenParen, tkCloseParen, tkOpenBrack, tkCloseBrack, tkOpenCurly, tkCloseCurly
    tkString, tkNumber, tkWord, tkSymbol
  
  CharacterTokenKind* = range[tkBackslash..tkCloseCurly]

  # ref to not copy on JS, not sure if improves performance on C
  TokenObj* = object
    when doLineColumn:
      line*, column*: int
    case kind*: TokenKind
    of tkString:
      content*: string
    of tkNumber:
      num*: NumberRepr
    of tkWord, tkSymbol:
      raw*: string
      quoted*: bool
    else: discard

when refToken:
  type Token* = ref TokenObj
else:
  type Token* = TokenObj

const
  CharacterTokens*: array[CharacterTokenKind, char] = [
    tkBackslash: '\\',
    tkDot: '.',
    tkComma: ',',
    tkColon: ':',
    tkSemicolon: ';',
    tkOpenParen: '(',
    tkCloseParen: ')',
    tkOpenBrack: '[',
    tkCloseBrack: ']',
    tkOpenCurly: '{',
    tkCloseCurly: '}'
  ]

  CharacterTokenSet* = block:
    var result: set[char]
    for sc in CharacterTokens:
      result.incl(sc)
    result

proc `$`*(token: Token): string =
  result = case token.kind
  of tkNone: "<none>"
  of tkWhitespace: " "
  of tkIndent: "<indent>"
  of tkIndentBack: "<indentback>"
  of tkNewLine: "\p"
  of tkBackslash..tkCloseCurly:
    $CharacterTokens[token.kind]
  of tkString: "\"" & token.content & "\""
  of tkNumber: $token.num
  of tkWord, tkSymbol: token.raw

proc `$`*(tokens: seq[Token]): string =
  var ind = 0
  for t in tokens:
    case t.kind
    of tkIndent:
      ind += 1
      result.add("  ")
    of tkIndentBack:
      ind -= 1
      result.setLen(result.len - 2)
    of tkNewLine:
      result.add("\p")
      let oldLen = result.len
      result.setLen(oldLen + ind * 2)
      for i in oldLen ..< result.len:
        result[i] = ' '
    else: result.add($t)

proc `==`*(tok1, tok2: Token): bool =
  when Token is ref:
    if tok1.isNil and tok2.isNil: return true
    if tok1.isNil xor tok2.isNil: return false
  if tok1.kind != tok2.kind: return false
  case tok1.kind
  of tkString: tok1.content == tok2.content
  of tkNumber: tok1.num == tok2.num
  of tkWord, tkSymbol: tok1.raw  == tok2.raw
  else: true
