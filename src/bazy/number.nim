type
  NumberKind* = enum Integer Floating Unsigned

  NumberReprObj* = object
    digits*: seq[byte]
    kind*: NumberKind
    negative*: bool
    exp*, bits*: int

when defined(js):
  type NumberRepr* = ref NumberReprObj
  proc `==`*(n1, n2: NumberRepr): bool =
    n1.isNil and n2.isNil or (n1.isNil == n2.isNil and n1[] == n2[])
else:
  type NumberRepr* = NumberReprObj

proc `$`*(number: NumberRepr): string =
  result = newStringOfCap(number.digits.len + 10)
  if number.negative:
    result.add('-')
  for d in number.digits:
    result.add(char('0'.byte + d))
  if number.kind == Floating and number.exp < 0 and -number.exp < number.digits.len:
    result.insert(".", number.negative.ord + number.digits.len + number.exp)
  elif number.exp != 0:
    if number.digits.len > 1:
      result.insert(".", number.negative.ord + 1)
    result.add('e')
    result.add($(number.exp + number.digits.len - 1))
