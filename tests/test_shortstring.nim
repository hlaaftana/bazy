import bazy/language/shortstring

block:
  for s in ["", "a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg", "abcdefgh"]:
    block:
      let ss = s.toShortString
      doAssert $ss == s
      for i in 0 ..< s.len:
        doAssert s[i] == ss[i]
    block:
      let ss = s.toShortString(optimized = false)
      doAssert $ss == s
      for i in 0 ..< s.len:
        doAssert s[i] == ss[i]
  doAssert short"ab" < short"abc"
  doAssert short"ab" < short"ac"
  doAssert short"ab" < short"bb"
  doAssert toShortString"ab" < toShortString"abc"
  doAssert toShortString"ab" < toShortString"ac"
  doAssert toShortString"ab" < toShortString"bb"
