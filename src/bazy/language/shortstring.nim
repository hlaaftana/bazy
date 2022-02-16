type
  ShortString* = distinct uint

proc `==`*(a, b: ShortString): bool {.borrow.}
proc `<`*(a, b: ShortString): bool {.borrow.}

const shortStringMaxSize* = sizeof(ShortString) div sizeof(char)
const charBits = sizeof(char) * 8

template get(ss: ShortString, i: int): char =
  char(
    (ss.uint shr (i * charBits)) and
      high(char).uint)

template set(ss: var ShortString, i: int, c: char) =
  ss = ShortString(ss.uint or
    (c.uint shl (i * charBits)))

proc `[]`*(ss: ShortString, i: int): char {.inline.} =
  rangeCheck i >= 0 and i < shortStringMaxSize
  get(ss, i)

proc `[]=`*(ss: var ShortString, i: int, c: char) {.inline.} =
  rangeCheck i >= 0 and i < shortStringMaxSize
  set(ss, i, c)

proc `[]`*(ss: ShortString, sl: Slice[int]): ShortString {.inline.} =
  rangeCheck sl.a >= 0 and sl.a < shortStringMaxSize and sl.b >= 0 and sl.b < shortStringMaxSize
  # use left shift overflow instead of unsigned 1 - overflow
  ShortString((ss.uint shl (sl.a * charBits)) shr (sl.b * charBits))

proc `[]=`*(ss: var ShortString, sl: Slice[int], ss2: ShortString) {.inline.} =
  rangeCheck sl.a >= 0 and sl.a < shortStringMaxSize and sl.b >= 0 and sl.b < shortStringMaxSize
  for i in sl:
    ss[i] = ss2[i - sl.a]

proc len*(ss: ShortString): int =
  # unrolled loop
  {.push rangeChecks: off.}
  template doIndex(i: int) =
    if get(ss, i) == char(0):
      return i
  doIndex 0
  doIndex 1
  doIndex 2
  doIndex 3
  doIndex 4
  doIndex 5
  doIndex 6
  doIndex 7
  return 8
  {.pop.}

template `[]`*(ss: ShortString, i: BackwardsIndex): char =
  ss[ss.len - i.int]

template `[]=`*(ss: var ShortString, i: BackwardsIndex, c: char) =
  ss[ss.len - i.int] = c

iterator items*(ss: ShortString): char =
  # not unrolled because nim doesnt allow return
  {.push rangeChecks: off.}
  var i = 0
  while i < shortStringMaxSize:
    let c = get(ss, i)
    if c == char(0):
      break
    yield c
    inc i
  {.pop.}

when not defined(js) and not defined(nimscript):
  when defined(gcc) or defined(llvm_gcc) or defined(clang):
    when shortStringMaxSize == 2:
      proc swapEndian(a: uint): uint {.
          importc: "__builtin_bswap16", nodecl, noSideEffect.}
    elif shortStringMaxSize == 4:
      proc swapEndian(a: uint): uint {.
          importc: "__builtin_bswap32", nodecl, noSideEffect.}
    elif shortStringMaxSize == 8:
      proc swapEndian(a: uint): uint {.
          importc: "__builtin_bswap64", nodecl, noSideEffect.}
  elif defined(icc):
    when shortStringMaxSize == 2:
      proc swapEndian(a: uint): uint {.
          importc: "_bswap16", nodecl, noSideEffect.}
    elif shortStringMaxSize == 4:
      proc swapEndian(a: uint): uint {.
          importc: "_bswap", nodecl, noSideEffect.}
    elif shortStringMaxSize == 8:
      proc swapEndian(a: uint): uint {.
          importc: "_bswap64", nodecl, noSideEffect.}
  elif defined(vcc):
    when shortStringMaxSize == 2:
      proc swapEndian(a: uint): uint {.
          importc: "_byteswap_ushort", nodecl, header: "<intrin.h>", noSideEffect.}
    elif shortStringMaxSize == 4:
      proc swapEndian(a: uint): uint {.
          importc: "_byteswap_ulong", nodecl, header: "<intrin.h>", noSideEffect.}
    elif shortStringMaxSize == 8:
      proc swapEndian(a: uint): uint {.
          importc: "_byteswap_uint64", nodecl, header: "<intrin.h>", noSideEffect.}
  when declared(swapEndian):
    template toLittleEndian(x: uint): uint =
      when cpuEndian == bigEndian:
        swapEndian(x)
      else:
        x

proc `$`*(ss: ShortString): string =
  when nimvm:
    result = newStringOfCap(sizeof(ShortString))
    for c in ss.items:
      result.add(c)
  else:
    when defined(js) or defined(nimscript) or (cpuEndian == bigEndian and not declared(swapEndian)):
      result = newStringOfCap(sizeof(ShortString))
      for c in ss.items:
        result.add(c)
    else:
      # this should be faster than adding one at a time, but we still have to calculate length
      if ss.uint == 0:
        result = ""
      else:
        result = newString(ss.len)
        cast[ptr uint](addr result[0])[] = toLittleEndian(ss.uint)

iterator mitems*(ss: var ShortString): var char =
  {.push rangeChecks: off.}
  var i = 0
  while i < shortStringMaxSize:
    var c = get(ss, i)
    if c == char(0):
      break
    yield addr(c)[]
    ss[i] = c
    inc i
  {.pop.}

proc add*(ss: var ShortString, c: char) =
  {.push rangeChecks: off.}
  var i = 0
  while i < shortStringMaxSize:
    let c = get(ss, i)
    if c == char(0):
      set(ss, i, c)
      return
    inc i
  {.pop.}
  assert false, "string " & $ss & " is full"

proc toShortString*(s: openarray[char], optimized: static bool = true): ShortString =
  rangeCheck s.len <= shortStringMaxSize
  when nimvm:
    for i, c in s:
      result[i] = c
  else:
    when defined(js) or defined(nimscript) or not optimized or (cpuEndian == bigEndian and not declared(swapEndian)):
      for i, c in s:
        result[i] = c
    else:
      if s.len == 0:
        # bypass nil
        result = ShortString(0)
      else:
        # this might still be invalid memory access
        #ShortString(cast[ptr uint](unsafeAddr s[0])[] and
        #  # use unsigned to bypass overflow
        #  (1u shl (result.len.uint * charBits.uint + 1u) - 1u))
        # XXX benchmark if this is faster
        let offset = shortStringMaxSize - s.len
        result = ShortString(
          (cast[ptr uint](unsafeAddr s[0])[].toLittleEndian shl
            (offset * charBits)) shr
              (offset * charBits))

template short*(s: static string): ShortString =
  toShortString(s)

when isMainModule:
  for s in ["", "a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg", "abcdefgh"]:
    block:
      let ss = s.toShortString
      assert $ss == s
      for i in 0 ..< s.len:
        assert s[i] == ss[i]
    block:
      let ss = s.toShortString(optimized = false)
      assert $ss == s
      for i in 0 ..< s.len:
        assert s[i] == ss[i]
  assert short"ab" < short"abc"
  assert short"ab" < short"ac"
  assert short"ab" < short"bb"
  assert toShortString"ab" < toShortString"abc"
  assert toShortString"ab" < toShortString"ac"
  assert toShortString"ab" < toShortString"bb"
