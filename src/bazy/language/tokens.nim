import number, shortstring, ../defines

type
  # TODO: add shortword/shortsymbol tokens with shortstrings for faster operator/keyword handling
  TokenKind* = enum
    tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewLine
    tkBackslash, tkDot, tkComma, tkColon, tkSemicolon
    tkOpenParen, tkCloseParen, tkOpenBrack, tkCloseBrack, tkOpenCurly, tkCloseCurly
    tkString, tkNumber, tkWord, tkSymbol
  
  CharacterTokenKind* = range[tkBackslash..tkCloseCurly]

  # XXX should also have filename and ending column
  TokenInfo* = tuple[line, column: int]

  TokenObj* = object
    when doLineColumn:
      info*: TokenInfo
    case kind*: TokenKind
    of tkString:
      content*: string
    of tkNumber:
      num*: NumberRepr
    of tkWord:
      raw*: string
      quoted*: bool
    of tkSymbol:
      short*: ShortString
    else: discard

# reduces copying on JS, not sure if improves performance on C
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
  of tkWord: token.raw
  of tkSymbol: $token.short

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
  of tkWord: tok1.raw == tok2.raw
  of tkSymbol: tok1.short == tok2.short
  else: true
