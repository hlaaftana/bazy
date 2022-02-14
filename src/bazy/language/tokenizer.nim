import number, tokens, ../defines, std/strutils

when useUnicode:
  import std/unicode

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

  template isWhitespace(r: Rune): bool = r in Whitespace

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

proc defaultOptions*(): TokenizerOptions =
  TokenizerOptions(
    symbolWords: @["do", "and", "or", "is", "as", "not", "in", "div", "mod", "xor"],
    stringBackslashEscape: true,
    stringQuoteEscape: true,
    backslashBreakNewline: true,
    commaIgnoreIndent: true)

proc newTokenizer*(str: sink string = "", options = defaultOptions()): Tokenizer =
  Tokenizer(str: str, options: options)

proc resetPos*(tz: var Tokenizer) =
  assert tz.previousPos != -1, "no previous position to reset to"
  tz.pos = tz.previousPos
  tz.previousPos = -1
  when doLineColumn:
    tz.cl = tz.previousCol
    if tz.currentRune == '\n':
      dec tz.ln

proc nextRune*(tz: var Tokenizer) =
  ## converts \r\n to \n and updates line and column
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
      if c != '\\' and c != quote:
        result.add('\\')
      result.add(c)
      escaped = false
    elif tz.options.stringBackslashEscape and c == '\\':
      escaped = true
    elif c == quote:
      if tz.options.stringQuoteEscape and tz.pos < tz.str.len and tz.str[tz.pos] == quote:
        tz.nextRune()
        result.add(c)
      else:
        return
    else:
      result.add(c)

proc recordNumber*(tz: var Tokenizer, negative = false): NumberRepr =
  type Stage = enum
    inBase, inDecimalStart, inDecimal, inExpStart, inExp, inExpNeg, inBits

  result = NumberRepr(negative: negative)

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
      elif result.exp < 0 and -result.exp < result.digits.len:
        # remove excessive zeros, ie 10000e-3 is simplified to 10
        result.exp = 0
        result.digits.setLen(result.digits.len + result.exp)

  for c in tz.runes:
    case stage
    of inBase:
      case c.asChar
      of '0'..'9':
        if c == '0':
          inc lastZeros
        else:
          lastZeros = 0
        result.digits.add(c.byte - '0'.byte)
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
        result.digits.add(c.byte - '0'.byte)
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

const NonSymbolChars = Whitespace + Digits + CharacterTokenSet + {'_', '\'', '"', '`', '#'}

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
  type IndentContext = object
    levels: seq[int]
    level: int

  result = newSeq[Token]()
  var
    lastKind: TokenKind
    indent: int
    recordingIndent, comment: bool
    indentContexts = @[IndentContext()]

  template dropLast() =
    let l1 = result.len - 1
    result.setLen(l1)
    lastKind = if unlikely(l1 == 0): tkNone else: result[l1 - 1].kind

  template add(t: Token) =
    var tok = t
    when doLineColumn:
      tok.line = ln
      tok.column = cl
    lastKind = tok.kind
    result.add(tok)

  template add(tt: TokenKind) =
    add Token(kind: tt)

  for c in tz.runes:
    when doLineColumn:
      let (ln, cl) = (tz.ln, tz.cl)
    if comment:
      if c == '\n':
        comment = false
      else: continue

    let w = c.isWhitespace

    if recordingIndent and c != '\n':
      case c.asChar
      of '\t':
        inc indent, 4
        continue
      of '#':
        indent = 0
        # recordingIndent still true
        comment = true
        continue
      elif w:
        inc indent
        continue
      else:
        template indentLevels: untyped = indentContexts[^1].levels
        template indentLevel: untyped = indentContexts[^1].level
        let diff = indent - indentLevel
        if diff < 0:
          var d = -diff
          var i = indentLevels.len
          while i > 0:
            dec i
            let indt = indentLevels[i]
            dec d, indt
            dec indentLevel, indt
            add tkIndentBack
            if d < 0:
              dec indentLevel, d
              indentLevels[^1] = -d
              add tkIndent
              break
            indentLevels.setLen(indentLevels.high)
            if d == 0: break
        elif diff > 0:
          indentLevels.add(diff)
          inc indentLevel, diff
          add tkIndent
        indent = 0
        recordingIndent = false

    if w:
      if c == '\n':
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
          add(tkNewLine)
        recordingIndent = not breakIndent
      elif lastKind != tkWhitespace:
        add(tkWhitespace)
    elif c.isAlpha or c == '_':
      # identifier start
      tz.resetPos()
      let word = recordWord(tz)
      if word in tz.options.symbolWords:
        add Token(kind: tkSymbol, raw: word)
      else:
        add Token(kind: tkWord, raw: word)
    elif c.int32 > 127:
      # non-space and non-alpha unicode char, so symbol start
      tz.resetPos()
      add Token(kind: tkSymbol, raw: recordSymbol(tz))
    else:
      let ch = c.char
      template doubleChar(k: TokenKind) =
        if lastKind == k:
          dropLast()
          tz.resetPos()
          add Token(kind: tkSymbol, raw: CharacterTokens[k] & recordSymbolPlus(tz, ch))
        else:
          add k
      template openDelim(kind: TokenKind) =
        indentContexts.add(IndentContext())
        add kind
      template closeDelim(kind: TokenKind) =
        if indentContexts.len > 1:
          indentContexts.setLen(indentContexts.high)
        add kind
      case ch
      of '#': comment = true
      of '\\': add tkBackslash
      of '.': doubleChar tkDot
      of ',': add tkComma
      of ':': doubleChar tkColon
      of ';': add tkSemicolon
      of '(': openDelim tkOpenParen
      of '[': openDelim tkOpenBrack
      of '{': openDelim tkOpenCurly
      of ')': closeDelim tkCloseParen
      of ']': closeDelim tkCloseBrack
      of '}': closeDelim tkCloseCurly
      of '\'', '"', '`':
        let s = recordString(tz, ch)
        if c == '`':
          add Token(kind: tkSymbol, raw: s, quoted: true)
        else:
          add Token(kind: tkString, content: s)
      of '0'..'9':
        tz.resetPos()
        let n = recordNumber(tz)
        add Token(kind: tkNumber, num: n)
      of '+', '-':
        if tz.pos < tz.str.len and tz.str[tz.pos] in {'0'..'9'}:
          add Token(kind: tkNumber, num: recordNumber(tz, ch == '-'))
        else:
          tz.resetPos()
          add Token(kind: tkSymbol, raw: recordSymbol(tz))
      else:
        # non-alpha, so symbol start
        tz.resetPos()
        if lastKind in {tkDot, tkColon}:
          let ch = CharacterTokens[lastKind]
          dropLast()
          add Token(kind: tkSymbol, raw: ch & recordSymbol(tz))
        else:
          add Token(kind: tkSymbol, raw: recordSymbol(tz))

proc tokenize*(str: string): seq[Token] =
  var tokenizer = newTokenizer(str)
  tokenize(tokenizer)

when isMainModule:
  import times, sequtils
  template bench*(body) =
    let a = cpuTime()
    for i in 1..10000000: body
    let b = cpuTime()
    echo "took ", b - a
  let s = "foo(\"a\" \"abcd\")"
  echo system.`$`(tokenize(s))
  echo $tokenize(s).mapIt(it.kind)
