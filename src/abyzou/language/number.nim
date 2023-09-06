type
  NumberKind* = enum Integer, Floating, Unsigned

  NumberReprObj* = object
    digits*: seq[byte]
    kind*: NumberKind
    negative*: bool
    exp*: int16
    bits*: uint8

when defined(js):
  import skinsuit/equals
  type NumberRepr* = ref NumberReprObj
  equals *NumberRepr
else:
  type NumberRepr* = NumberReprObj

when false: {.hint: $sizeof(NumberRepr).}

proc `$`*(number: NumberRepr): string =
  var exponent: string
  let dotIndex =
    if number.kind == Floating and number.exp < 0 and -number.exp < number.digits.len:
      number.digits.len + number.exp - 1
    elif number.exp != 0:
      exponent = $(number.exp + number.digits.len - 1)
      if number.digits.len > 1:
        0
      else:
        -1
    else:
      -2
  result = newStringOfCap(number.negative.ord +
    number.digits.len +
    (dotIndex >= 0).ord +
    (if exponent.len != 0: exponent.len + 1 else: 0)) # exact length
  if number.negative:
    result.add('-')
  for i, d in number.digits:
    result.add(char('0'.byte + d))
    if i == dotIndex: result.add('.') 
  if exponent.len != 0:
    result.add('e')
    result.add(exponent)
