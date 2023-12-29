## array that takes up more memory but is faster with double pointer indirection
type Array*[T] = object
  case lenClass: range[0 .. 0b111]
  of 0: discard
  of 1: one: T
  of 2: two: (T, T)
  of 3: three: (T, T, T)
  else: rest: seq[T]

proc len*[T](arr: Array[T]): int =
  case arr.lenClass
  of 0, 1, 2, 3: arr.lenClass.int
  else: arr.rest.len

iterator items*[T](arr: Array[T]): T =
  case arr.lenClass
  of 0: discard
  of 1: yield arr.one
  of 2:
    yield arr.two[0]
    yield arr.two[1]
  of 3:
    yield arr.three[0]
    yield arr.three[1]
    yield arr.three[2]
  else:
    for x in arr.rest.items: yield x

iterator mitems*[T](arr: var Array[T]): var T =
  case arr.lenClass
  of 0: discard
  of 1: yield arr.one
  of 2:
    yield arr.two[0]
    yield arr.two[1]
  of 3:
    yield arr.three[0]
    yield arr.three[1]
    yield arr.three[2]
  else:
    for x in arr.rest.mitems: yield x

iterator pairs*[T](arr: Array[T]): (int, T) =
  case arr.lenClass
  of 0: discard
  of 1: yield (0, arr.one)
  of 2:
    yield (0, arr.two[0])
    yield (1, arr.two[1])
  of 3:
    yield (0, arr.three[0])
    yield (1, arr.three[1])
    yield (2, arr.three[2])
  else:
    for x in arr.rest.pairs: yield x

iterator mpairs*[T](arr: Array[T]): (int, var T) =
  case arr.lenClass
  of 0: discard
  of 1: yield (0, arr.one)
  of 2:
    yield (0, arr.two[0])
    yield (1, arr.two[1])
  of 3:
    yield (0, arr.three[0])
    yield (1, arr.three[1])
    yield (2, arr.three[2])
  else:
    for x in arr.rest.mpairs: yield x

proc newArray*[T](len: int): Array[T] =
  result = Array[T](lenClass: min(4, len))
  case result.lenClass
  of 0, 1, 2, 3: discard
  else: result.rest.newSeq(len)

proc toArray*[T](x: openarray[T]): Array[T] =
  result = Array[T](lenClass: min(4, x.len))
  case result.lenClass
  of 0: discard
  of 1: result.one = x[0]
  of 2: result.two = (x[0], x[1])
  of 3: result.three = (x[0], x[1], x[2])
  else: result.rest = @x

proc toSeq*[T](arr: Array[T]): seq[T] =
  case arr.lenClass
  of 0: @[]
  of 1: @[arr.one]
  of 2: @[arr.two[0], arr.two[1]]
  of 3: @[arr.three[0], arr.three[1], arr.three[2]]
  else: arr.rest
converter toSeqConverter*[T](arr: Array[T]): seq[T] = toSeq(arr)

proc `[]`*[T](arr: Array[T], index: int): T =
  case arr.lenClass
  of 0: raise newException(IndexDefect, "array is empty but got index " & $index)
  of 1:
    if index != 0:
      raise newException(IndexDefect, "array has 1 element but got index " & $index)
    result = arr.one
  of 2:
    case index
    of 0: result = arr.two[0]
    of 1: result = arr.two[1]
    else: raise newException(IndexDefect, "array has 2 elements but got index " & $index)
  of 3:
    case index
    of 0: result = arr.three[0]
    of 1: result = arr.three[1]
    of 2: result = arr.three[2]
    else: raise newException(IndexDefect, "array has 3 elements but got index " & $index)
  else:
    result = arr.rest[index]

proc `[]`*[T](arr: var Array[T], index: int): var T =
  case arr.lenClass
  of 0: raise newException(IndexDefect, "array is empty but got index " & $index)
  of 1:
    if index != 0:
      raise newException(IndexDefect, "array has 1 element but got index " & $index)
    result = arr.one
  of 2:
    case index
    of 0: result = arr.two[0]
    of 1: result = arr.two[1]
    else: raise newException(IndexDefect, "array has 2 elements but got index " & $index)
  of 3:
    case index
    of 0: result = arr.three[0]
    of 1: result = arr.three[1]
    of 2: result = arr.three[2]
    else: raise newException(IndexDefect, "array has 3 elements but got index " & $index)
  else:
    result = arr.rest[index]

proc `[]=`*[T](arr: var Array[T], index: int, value: sink T) =
  case arr.lenClass
  of 0: raise newException(IndexDefect, "array is empty but got index " & $index)
  of 1:
    if index != 0:
      raise newException(IndexDefect, "array has 1 element but got index " & $index)
    arr.one = value
  of 2:
    case index
    of 0: arr.two[0] = value
    of 1: arr.two[1] = value
    else: raise newException(IndexDefect, "array has 2 elements but got index " & $index)
  of 3:
    case index
    of 0: arr.three[0] = value
    of 1: arr.three[1] = value
    of 2: arr.three[2] = value
    else: raise newException(IndexDefect, "array has 3 elements but got index " & $index)
  else:
    arr.rest[index] = value

import hashes

proc hash*[T](arr: Array[T]): Hash =
  result = result !& hash(arr.lenClass)
  for x in arr:
    result = result !& hash(x)
  result = !$ result

proc `==`*[T](a, b: Array[T]): bool =
  if a.lenClass != b.lenClass: return false
  case a.lenClass
  of 0: result = true
  of 1: result = a.one == b.one
  of 2: result = a.two == b.two
  of 3: result = a.three == b.three
  else: result = a.rest == b.rest
