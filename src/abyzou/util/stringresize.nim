when not declared(capacity):
  template capacity(s: string): int = high(int)

proc smartResizeAdd*(s: var string, a: string, freeBefore: int): bool =
  let cap = s.capacity
  if s.len + a.len > cap:
    let realSLen = s.len - freeBefore
    if freeBefore != 0: # would make this noop
      for i in 0 ..< realSLen:
        s[i] = s[i + freeBefore]
    s.setLen(realSLen + a.len)
    for i in 0 ..< a.len:
      s[i + realSLen] = a[i]
    result = true
  else:
    s.add(a)
    result = false
