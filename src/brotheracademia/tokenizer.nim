import strutils, unicode

type
  TokenKind* = enum
    tkNone, tkWhitespace, tkIndent, tkIndentBack, tkNewLine
    tkBackslash, tkDot, tkComma, tkColon, tkSemicolon
    tkOpenParen, tkCloseParen, tkOpenBrack, tkCloseBrack, tkOpenCurly, tkCloseCurly
    tkString, tkNumber, tkWord, tkSymbol
  
  SpecialCharacterKind* = range[tkBackslash..tkCloseCurly]

  NumberToken* = object
    base*: seq[byte]
    floating*, negative*: bool
    exp*, bits*: int

  Token* = object
    line*, column*: int
    case kind*: TokenKind
    of tkString:
      content*: string
    of tkNumber:
      num*: NumberToken
    of tkWord, tkSymbol:
      raw*: string
      quoted*: bool
    #of tkNewLine:
    #  ampersand*: bool
    else: discard

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
  result = newStringOfCap(number.base.len + 2)
  if number.negative:
    result.add('-')
  for d in number.base:
    result.add(('0'.byte + d).char)
  if number.floating and -number.exp < number.base.len:
    result.insert(".", number.base.len + number.exp)
  elif number.exp != 0:
    result.add('e')
    result.add($number.exp)

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

proc `==`*(tok1, tok2: Token): bool =
  if tok1.kind != tok2.kind: return
  case tok1.kind
  of tkString: tok1.content == tok2.content
  of tkNumber: tok1.num == tok2.num
  of tkWord, tkSymbol: tok1.raw  == tok2.raw
  #of tkNewline: not (tok1.ampersand xor tok2.ampersand)
  else: true

iterator rev[T](a: openArray[T]): T {.inline.} =
  var i = a.len
  while i > 0:
    dec(i)
    yield a[i]

type Tokenizer* = object
  str*: string
  symbolWords*: seq[string]
  pos*: int
  ln*, cl*: int

template getChar*(tz: Tokenizer, offset: int = 0): char = tz.str[tz.pos + offset]

template getStr*(tz: Tokenizer, length: int, offset: int = 0): char = tz.str[tz.pos + offset ..< tz.pos + offset + length]

template matchNext*(tz: Tokenizer, pat: char, offset: int = 0): bool =
  tz.pos + offset < len(tz.str) and tz.getChar(offset) == pat

template matchNext*(tz: Tokenizer, pat: set[char], offset: int = 0): bool =
  tz.pos + offset < len(tz.str) and tz.getChar(offset) in pat

template matchNext*(tz: Tokenizer, pat: string, offset: int = 0): bool =
  tz.pos + offset + pat.len <= len(tz.str) and (block:
    var res = true
    for i in 0..<pat.len:
      if tz.getChar(i + offset) != pat[i]:
        res = false
        break
    res) # getStr copies, maybe unroll this loop for static[string]

template matchNextSkip*(tz: var Tokenizer, pat: char): bool =
  tz.pos < len(tz.str) and tz.getChar == pat and (inc tz.pos; true)

template matchNextSkip*(tz: var Tokenizer, pat: set[char]): bool =
  tz.pos < len(tz.str) and tz.getChar in pat and (inc tz.pos; true)

template matchNextSkip*(tz: var Tokenizer, pat: string): bool =
  tz.pos + pat.len <= len(tz.str) and (block:
    var res = true
    for i in 0..<pat.len:
      if tz.getChar(i) != pat[i]:
        res = false
        break
    res) and (tz.pos += pat.len; true)

iterator chars*(tz: var Tokenizer, offset: int = 0): char =
  while tz.pos + offset < len(tz.str):
    yield tz.getChar(offset)
    inc tz.pos

iterator runes*(tz: var Tokenizer): Rune =
  var
    result: Rune
  while tz.pos < len(tz.str):
    if tz.matchNextSkip("\c\L"):
      yield Rune '\L'
    else:
      fastRuneAt(tz.str, tz.pos, result, true)
      yield result

proc contains*(s: set[char], r: Rune): bool {.inline.} =
  r.int32 < 128i32 and r.char in s

proc `==`*(s: char, r: Rune): bool {.inline.} =
  r.int32 < 128i32 and r.char == s

template `==`*(r: Rune, s: char): bool = s == r

proc recordString*(tz: var Tokenizer, quote: char): string =
  var
    escaped = false
    recordU = false
    uLevel = 16
    uNum = 0
    startedU = -1
  for c in tz.chars:
    if startedU != -1:
      if recordU:
        case c
        of '}':
          recordU = false
          result.add($Rune(uNum))
          uNum = 0
          uLevel = 16
          startedU = -1
        of '_':
          discard
        of '0'..'9':
          uNum = uNum * uLevel + int(c.byte - '0'.byte)
        of 'a'..'f':
          uNum = uNum * uLevel + 10 + int(c.byte - 'a'.byte)
        of 'A'..'F':
          uNum = uNum * uLevel + 10 + int(c.byte - 'A'.byte)
        else:
          dec tz.pos
          result.add(tz.str[startedU..tz.pos])
          recordU = false
          uNum = 0
          uLevel = 16
          startedU = -1
      else:
        case c
        of 'x', 'X': uLevel = 16
        of 'o', 'O': uLevel = 8
        of 'd', 'D': uLevel = 10
        of 'b', 'B': uLevel = 2
        of '{': recordU = true
        else:
          recordU = false
          startedU = -1
          result.add('\\')
          result.add('u')
          result.add(c)
    elif escaped:
      if c == 'u':
        startedU = tz.pos - 1
      else:
        let ch = case c
          of 't': '\t'
          of '"': '"'
          of '\'': '\''
          of '`': '`'
          of '\\': '\\'
          of 'r': '\r'
          of 'n': '\l'
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
    else:
      if c == quote:
        inc tz.pos
        return
      elif c == '\\':
        escaped = true
      else:
        result.add(c)

proc recordNumber*(tz: var Tokenizer, negative = false): NumberToken =
  type Stage = enum
    inBase, inDecimal, inExpStart, inExp, inExpNeg, inBits

  var
    stage = inBase
    lastZeros: Natural = 0
  
  result.negative = negative
  
  defer:
    if not result.floating:
      if lastZeros < -result.exp:
        result.floating = true
      elif result.exp < 0:
        # remove excessive zeros, ie 10000e-3 is simplified to 10
        result.exp = 0
        result.base.setLen(result.base.len + result.exp)

  dec tz.pos
  for c in tz.chars:
    case stage
    of inBase:
      case c
      of '0'..'9':
        if c == '0':
          inc lastZeros
        else:
          lastZeros = 0
        result.base.add(c.byte - '0'.byte)
      of '.':
        result.floating = true
        stage = inDecimal
      of 'e', 'E':
        stage = inExpStart
      of 'i', 'I':
        result.floating = false
        stage = inBits
      of 'f', 'F':
        result.floating = true
        stage = inBits
      else:
        return
    of inDecimal:
      case c
      of '0'..'9':
        if c == '0':
          inc lastZeros
        else:
          lastZeros = 0
        result.base.add(c.byte - '0'.byte)
        dec result.exp
      of 'e', 'E':
        stage = inExpStart
      else:
        return
    of inExpStart:
      case c
      of '+':
        stage = inExp
      of '-':
        stage = inExpNeg
      of '0'..'9':
        stage = inExp
        continue
      else:
        dec tz.pos
        return
    of inExp, inExpNeg:
      case c
      of 'i', 'I':
        result.floating = false
        stage = inBits
      of 'f', 'F':
        result.floating = true
        stage = inBits
      of '0'..'9':
        let val = (c.byte - '0'.byte).int
        result.exp = result.exp * 10 + (if stage == inExpNeg: -val else: val)
      else:
        return
    of inBits:
      case c
      of '0'..'9':
        result.bits = (result.bits * 10) + (c.byte - '0'.byte).int
      else:
        dec tz.pos
        return

proc recordWord*(tz: var Tokenizer): string =
  dec tz.pos
  for c in runes(tz):
    if c == Rune('_') or (c.int < 128 and c.char.isDigit) or c.isAlpha:
      result.add(c)
    else:
      dec tz.pos
      return

proc recordSymbol*(tz: var Tokenizer): string =
  dec tz.pos
  for c in runes(tz):
    if c notin (Whitespace + Digits + SpecialCharacterSet + {'_', '\'', '"', '`', '#'}) and not c.isAlpha:
      result.add(c)
    else:
      dec tz.pos
      return

proc recordSymbolPlus*(tz: var Tokenizer, extra: char): string =
  dec tz.pos
  for c in runes(tz):
    if c == extra or (c notin (Whitespace + Digits + SpecialCharacterSet + {'_', '\'', '"', '`', '#'}) and not c.isAlpha):
      result.add(c)
    else:
      dec tz.pos
      return

proc tokenize*(tz: var Tokenizer): seq[Token] =
  result = newSeq[Token]()
  var
    #currentLineBars: int
    lastKind: TokenKind
    lastIndents: seq[int]
    lastIndentSum, indent: int
    recordingIndent, comment: bool

  template addToken(t: Token) =
    result.add(t)
    lastKind = t.kind

  template dropLast =
    let l1 = result.len - 1
    result.setLen(l1)
    lastKind = if unlikely(l1 == 0): tkNone else: result[l1 - 1].kind

  template addTokenOf(tt: TokenKind) =
    addToken(Token(kind: tt))

  for c in runes(tz):
    if comment:
      if c == Rune('\L'):
        comment = false
      else: continue

    let w = c in Whitespace

    if recordingIndent and c != '\L':
      if c == '\t'.Rune: inc indent, 4; continue
      elif w: inc indent; continue
      elif c != Rune('#'):
        let diff = indent - lastIndentSum
        if diff < 0:
          var d = -diff # 11
          for indt in lastIndents.rev:
            dec d, indt # -2
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
      if c == '\L':
        #if c == '\c' and str.len > i + 1 and str[i + 1] == '\L':
        #  inc i
        let r = high(result)
        for xi in countdown(r, 0):
          if result[xi].kind == tkWhitespace:
            continue
          elif result[xi].kind == tkBackslash:
            result.del(xi)
          break
        if r == high(result):
          addTokenOf(tkNewLine)
        recordingIndent = true
      elif lastKind != tkWhitespace:
        addTokenOf(tkWhitespace)
    elif c.isAlpha or c == Rune('_'):
      let word = recordWord(tz)
      if word in tz.symbolWords:
        addToken(Token(kind: tkSymbol, raw: word))
      else:
        addToken(Token(kind: tkWord, raw: word))
    elif c.int32 > 127:
      addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))
    else:
      let ch = c.char
      case ch
      of '#': comment = true
      of SpecialCharacterSet:
        let kind = TokenKind(low(SpecialCharacterKind).ord + find(SpecialCharacters, ch))
        if kind in {tkDot, tkColon, tkSemicolon} and lastKind == kind:
          dropLast
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
        let n = recordNumber(tz)
        addToken(Token(kind: tkNumber, num: n))
      of '+', '-':
        if tz.matchNextSkip({'0'..'9'}):
          addToken(Token(kind: tkNumber, num: recordNumber(tz, ch == '-')))
        else:
          addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))
      else: addToken(Token(kind: tkSymbol, raw: recordSymbol(tz)))

proc tokenize*(str: string): seq[Token] =
  var tokenizer = Tokenizer(str: str, pos: 0)
  tokenize(tokenizer)

when isMainModule:
  import times
  template bench*(body) =
    let a = cpuTime()
    for i in 1..10000000: body
    let b = cpuTime()
    echo "took ", b - a

  echo tokenize(";;")
  #echo tokenize(readFile("concepts/v2.ksmt2"))