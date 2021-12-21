const
  # options for testing performance
  # if using unicode in JS, turn bound checks off
  useUnicode = block: (const bazyUseUnicode {.booldefine.} = true; bazyUseUnicode)
  doLineColumn = block: (const bazyDoLineColumn {.booldefine.} = true; bazyDoLineColumn)
  refToken = block: (const bazyRefToken {.booldefine.} = defined(js); bazyRefToken)

import strutils

when useUnicode:
  import unicode

type
  NumberKind* = enum Integer Floating Unsigned

  NumberTokenObj* = object
    base*: seq[byte]
    kind*: NumberKind
    negative*: bool
    exp*, bits*: int

when defined(js):
  type NumberToken* = ref NumberTokenObj
else:
  type NumberToken* = NumberTokenObj

type
  TokenKind* = enum
    tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewLine
    tkBackslash, tkDot, tkComma, tkColon, tkSemicolon
    tkOpenParen, tkCloseParen, tkOpenBrack, tkCloseBrack, tkOpenCurly, tkCloseCurly
    tkString, tkNumber, tkWord, tkSymbol
  
  SpecialCharacterKind* = range[tkBackslash..tkCloseCurly]

  # ref to not copy on JS, not sure if improves performance on C
  TokenObj* = object
    when doLineColumn:
      line*, column*: int
    case kind*: TokenKind
    of tkString:
      content*: string
    of tkNumber:
      num*: NumberToken
    of tkWord, tkSymbol:
      raw*: string
      quoted*: bool
    else: discard

when refToken:
  type Token* = ref TokenObj
else:
  type Token* = TokenObj

const
  SpecialCharacters*: array[SpecialCharacterKind, char] = [
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

  SpecialCharacterSet* = block:
    var result: set[char]
    for sc in SpecialCharacters:
      result.incl(sc)
    result

proc `$`*(number: NumberToken): string =
  result = newStringOfCap(number.base.len + 10)
  if number.negative:
    result.add('-')
  for d in number.base:
    result.add(('0'.byte + d).char)
  if number.kind == Floating and number.exp < 0 and -number.exp < number.base.len:
    result.insert(".", number.negative.ord + number.base.len + number.exp)
  elif number.exp != 0:
    if number.base.len > 1:
      result.insert(".", number.negative.ord + 1)
    result.add('e')
    result.add($(number.exp + number.base.len - 1))

proc `$`*(token: Token): string =
  result = case token.kind
  of tkNone: "<none>"
  of tkWhitespace: " "
  of tkIndent: "<indent>"
  of tkIndentBack: "<indentback>"
  of tkNewLine: "\p"
  of tkBackslash..tkCloseCurly:
    $SpecialCharacters[token.kind]
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
      for _ in 1..(ind * 2):
        result.add(' ')
    else: result.add($t)

when NumberToken is ref:
  proc `==`*(n1, n2: NumberToken): bool =
    n1.isNil and n2.isNil or (n1.isNil == n2.isNil and n1[] == n2[])

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

when useUnicode:
  template asChar(r: Rune): char =
    char(r.int and 0xFF)
  
  template isChar(r: Rune): bool =
    r.int32 < 128i32

  proc contains(s: set[char], r: Rune): bool {.inline.} =
    r.isChar and r.char in s

  proc `==`(s: char, r: Rune): bool {.inline.} =
    r.isChar and r.char == s

  template `==`(r: Rune, s: char): bool = s == r
else:
  type Rune = char
  
  template asChar(r: Rune): char = r

  template isChar(r: Rune): bool = true

  template isAlpha(r: Rune): bool = r.isAlphaAscii

type
  TokenizerOptions* = object
    symbolWords*: seq[string]
    stringBackslashEscape*, stringQuoteEscape*: bool
    backslashBreakNewline*, commaIgnoreIndent*: bool

  Tokenizer* = ref object
    options*: TokenizerOptions
    str*: string
    currentRune*: Rune
    pos*, previousPos*: int
    when doLineColumn:
      ln*, cl*, previousCol*: int

proc newTokenizer*(): Tokenizer =
  Tokenizer(options: TokenizerOptions(
    symbolWords: @[],
    stringBackslashEscape: true,
    stringQuoteEscape: true,
    backslashBreakNewline: true,
    commaIgnoreIndent: true))

proc resetPos*(tz: var Tokenizer) =
  assert tz.previousPos != -1, "no previous position to reset to"
  tz.pos = tz.previousPos
  tz.previousPos = -1
  when doLineColumn:
    tz.cl = tz.previousCol
    if tz.currentRune == '\n':
      dec tz.ln

proc nextRune*(tz: var Tokenizer) =
  tz.previousPos = tz.pos
  when doLineColumn:
    tz.previousCol = tz.cl
  if tz.pos + 1 < len(tz.str) and
    tz.str[tz.pos .. tz.pos + 1] == "\r\n":
    tz.currentRune = Rune '\n'
    tz.pos += 2
    when doLineColumn:
      tz.ln += 1
      tz.cl = 0
  else:
    when useUnicode:
      fastRuneAt(tz.str, tz.pos, tz.currentRune, true)
    else:
      tz.currentRune = tz.str[tz.pos]
      inc tz.pos
    when doLineColumn:
      if tz.currentRune == '\n':
        tz.ln += 1
        tz.cl = 0
      else:
        tz.cl += 1

iterator runes*(tz: var Tokenizer): Rune =
  while tz.pos < len(tz.str):
    tz.nextRune()
    yield tz.currentRune

proc recordString*(tz: var Tokenizer, quote: char): string =
  var escaped = false
  for c in tz.runes:
    if escaped:
      if c notin {'\\', quote}:
        result.add('\\')
      result.add(c)
      escaped = false
    elif tz.options.stringBackslashEscape and c == '\\':
      escaped = true
    elif c == quote:
      if tz.options.stringQuoteEscape and tz.pos + 1 < tz.str.len and
        tz.str[tz.pos + 1] == quote:
        tz.nextRune()
        result.add(c)
      else:
        return
    else:
      result.add(c)

proc recordNumber*(tz: var Tokenizer, negative = false): NumberToken =
  type Stage = enum
    inBase, inDecimalStart, inDecimal, inExpStart, inExp, inExpNeg, inBits

  result = NumberToken(negative: negative)

  var
    stage = inBase
    lastZeros: Natural = 0
    recordedExp = 0
  
  var prevPos2: int
  when doLineColumn:
    var prevCol2: int
  
  defer:
    result.exp += recordedExp
    if result.kind != Floating:
      if lastZeros < -result.exp:
        result.kind = Floating
      elif result.exp < 0 and -result.exp < result.base.len:
        # remove excessive zeros, ie 10000e-3 is simplified to 10
        result.exp = 0
        result.base.setLen(result.base.len + result.exp)

  for c in tz.runes:
    case stage
    of inBase:
      case c.asChar
      of '0'..'9':
        if c == '0':
          inc lastZeros
        else:
          lastZeros = 0
        result.base.add(c.byte - '0'.byte)
      of '.':
        result.kind = Floating
        stage = inDecimalStart
        prevPos2 = tz.previousPos
        when doLineColumn:
          prevCol2 = tz.previousCol
      of 'e', 'E':
        stage = inExpStart
        prevPos2 = tz.previousPos
        when doLineColumn:
          prevCol2 = tz.previousCol
      of 'i', 'I':
        result.kind = Integer
        stage = inBits
      of 'u', 'U':
        result.kind = Unsigned
        stage = inBits
      of 'f', 'F':
        result.kind = Floating
        stage = inBits
      else:
        tz.resetPos()
        return
    of inDecimalStart:
      tz.resetPos()
      case c.asChar
      of '0'..'9':
        stage = inDecimal
      else:
        result.kind = Integer
        tz.pos = prevPos2
        when doLineColumn:
          tz.cl = prevCol2
        return
    of inDecimal:
      case c.asChar
      of '0'..'9':
        if c == '0':
          inc lastZeros
        else:
          lastZeros = 0
        result.base.add(c.byte - '0'.byte)
        dec result.exp
      of 'e', 'E':
        stage = inExpStart
        prevPos2 = tz.previousPos
        when doLineColumn:
          prevCol2 = tz.previousCol
      else:
        tz.resetPos()
        return
    of inExpStart:
      case c.asChar
      of '+':
        stage = inExp
      of '-':
        stage = inExpNeg
      of '0'..'9':
        stage = inExp
        tz.resetPos()
      else:
        tz.resetPos()
        tz.pos = prevPos2
        when doLineColumn:
          tz.cl = prevCol2
        return
    of inExp, inExpNeg:
      case c.asChar
      of '0'..'9':
        let val = (c.byte - '0'.byte).int
        recordedExp = recordedExp * 10 + (if stage == inExpNeg: -val else: val)
      of 'i', 'I':
        result.kind = Integer
        stage = inBits
      of 'u', 'U':
        result.kind = Unsigned
        stage = inBits
      of 'f', 'F':
        result.kind = Floating
        stage = inBits
      else:
        tz.resetPos()
        return
    of inBits:
      case c.asChar
      of '0'..'9':
        result.bits = (result.bits * 10) + (c.byte - '0'.byte).int
      else:
        tz.resetPos()
        return

proc recordWord*(tz: var Tokenizer): string =
  for c in tz.runes:
    if c == Rune('_') or (c.isChar and c.char.isDigit) or c.isAlpha:
      result.add(c)
    else:
      tz.resetPos()
      return

const NonSymbolChars = Whitespace + Digits + SpecialCharacterSet + {'_', '\'', '"', '`', '#'}

proc recordSymbol*(tz: var Tokenizer): string =
  for c in tz.runes:
    if c notin NonSymbolChars and not c.isAlpha:
      result.add(c)
    else:
      tz.resetPos()
      return

proc recordSymbolPlus*(tz: var Tokenizer, extra: char): string =
  for c in tz.runes:
    if c == extra or (c notin NonSymbolChars and not c.isAlpha):
      result.add(c)
    else:
      tz.resetPos()
      return

proc tokenize*(tz: var Tokenizer): seq[Token] =
  result = newSeq[Token]()
  var
    lastKind: TokenKind
    lastIndents: seq[int]
    lastIndentSum, indent: int
    recordingIndent, comment: bool

  template addToken(t: Token) =
    var tok = t
    when doLineColumn:
      tok.line = ln
      tok.column = cl
    result.add(tok)
    lastKind = tok.kind

  template dropLast() =
    let l1 = result.len - 1
    result.setLen(l1)
    lastKind = if unlikely(l1 == 0): tkNone else: result[l1 - 1].kind

  template addTokenOf(tt: TokenKind) =
    addToken(Token(kind: tt))

  for c in tz.runes:
    when doLineColumn:
      let (ln, cl) = (tz.ln, tz.cl)
    if comment:
      if c == Rune('\n'):
        comment = false
      else: continue

    let w = c in Whitespace

    if recordingIndent and c != '\n':
      if c == '\t'.Rune: inc indent, 4; continue
      elif w: inc indent; continue
      elif c == Rune('#'): indent = 0 # recordingIndent still true
      else:
        let diff = indent - lastIndentSum
        if diff < 0:
          var d = -diff
          var i = lastIndents.len
          while i > 0:
            dec i
            let indt = lastIndents[i]
            dec d, indt
            dec lastIndentSum, indt
            addTokenOf(tkIndentBack)
            if d < 0:
              dec lastIndentSum, d
              lastIndents[lastIndents.high] = -d
              addTokenOf(tkIndent)
              break
            lastIndents.setLen(lastIndents.high)
            if d == 0: break
        elif diff > 0:
          lastIndents.add(diff)
          inc lastIndentSum, diff
          addTokenOf(tkIndent)
        indent = 0
        recordingIndent = false

    if w:
      if c == '\n': # \r\n -> \n in runes iterator
        let r = high(result)
        var breakIndent = false
        for xi in countdown(r, 0):
          case result[xi].kind
          of tkWhitespace:
            continue
          of tkBackslash:
            if tz.options.backslashBreakNewline:
              result.del(xi)
              breakIndent = true
          of tkColon:
            if tz.options.commaIgnoreIndent:
              breakIndent = true
          else: discard
          break
        if r == high(result):
          addTokenOf(tkNewLine)
        recordingIndent = not breakIndent
      elif lastKind != tkWhitespace:
        addTokenOf(tkWhitespace)
    elif c.isAlpha or c == Rune('_'):
      tz.resetPos()
      let word = recordWord(tz)
      if word in tz.options.symbolWords:
        addToken(Token(kind: tkSymbol, raw: word))
      else:
        addToken(Token(kind: tkWord, raw: word))
    elif c.int32 > 127:
      tz.resetPos()
      addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))
    else:
      let ch = c.char
      case ch
      of '#': comment = true
      of SpecialCharacterSet:
        let kind = TokenKind(low(SpecialCharacterKind).ord + find(SpecialCharacters, ch))
        if kind in {tkDot, tkColon, #[tkSemicolon]#} and lastKind == kind:
          dropLast()
          tz.resetPos()
          addToken(Token(kind: tkSymbol, raw: ch & recordSymbolPlus(tz, ch)))
        else:
          addTokenOf(kind)
      of '\'', '"', '`':
        let s = recordString(tz, ch)
        if c == '`':
          addToken(Token(kind: tkSymbol, raw: s, quoted: true))
        else:
          addToken(Token(kind: tkString, content: s))
      of '0'..'9':
        tz.resetPos()
        let n = recordNumber(tz)
        addToken(Token(kind: tkNumber, num: n))
      of '+', '-':
        if tz.pos < tz.str.len and tz.str[tz.pos] in {'0'..'9'}:
          addToken(Token(kind: tkNumber, num: recordNumber(tz, ch == '-')))
        else:
          tz.resetPos()
          addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))
      elif lastKind in {tkDot, tkColon}:
        let ch = SpecialCharacters[lastKind]
        dropLast()
        tz.resetPos()
        addToken(Token(kind: tkSymbol, raw: ch & recordSymbol(tz)))
      else:
        tz.resetPos()
        addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))

proc tokenize*(str: string): seq[Token] =
  var tokenizer = newTokenizer()
  tokenizer.str = str
  tokenize(tokenizer)

when isMainModule:
  import times
  template bench*(body) =
    let a = cpuTime()
    for i in 1..10000000: body
    let b = cpuTime()
    echo "took ", b - a
