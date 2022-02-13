type
  Array*[T] = object
    length*: int
    data*: ptr UncheckedArray[T]

  LengthEmbeddedArray*[L, T] = object
    data: pointer

  ShortArray*[T] = LengthEmbeddedArray[byte, T]

proc len*[T](x: Array[T]): int {.inline.} =
  x.length

proc len*[L, T](x: LengthEmbeddedArray[L, T]): int {.inline.} =
  cast[ptr UncheckedArray[L]](x.data)[0].int

proc `[]`*[T](x: Array[T], i: int): lent T {.inline.} =
  x.data[i]

proc `[]`*[T](x: var Array[T], i: int): var T {.inline.} =
  x.data[i]

proc `[]=`*[T](x: var Array[T], i: int, val: sink T) {.inline.} =
  x.data[i] = val

proc `[]`*[L, T](x: LengthEmbeddedArray[L, T], i: int): lent T {.inline.} =
  cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L).uint)[i]

proc `[]`*[L, T](x: var LengthEmbeddedArray[L, T], i: int): var T {.inline.} =
  cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L).uint)[i]

proc `[]=`*[L, T](x: var LengthEmbeddedArray[L, T], i: int, val: sink T) {.inline.} =
  cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L).uint)[i] = val

iterator items*[T](x: Array[T]): T =
  let L = x.len
  for i in 0 ..< L:
    yield x[i]

iterator mitems*[T](x: var Array[T]): var T =
  let L = x.len
  for i in 0 ..< L:
    yield x[i]

iterator items*[L, T](x: LengthEmbeddedArray[L, T]): T =
  let len = x.len
  for i in 0 ..< len:
    yield x[i]

iterator mitems*[L, T](x: var LengthEmbeddedArray[L, T]): var T =
  let len = x.len
  for i in 0 ..< len:
    yield x[i]

when not defined(js):
  proc `=destroy`*[T](x: var Array[T]) =
    if not x.data.isNil:
      for i in 0..<x.length: `=destroy`(x.data[i])
      dealloc(x.data)

  proc `=destroy`*[L, T](x: var LengthEmbeddedArray[L, T]) =
    if not x.data.isNil:
      for a in x.mitems:
        `=destroy`(a)
      dealloc(x.data)

  proc `=trace`*[T](x: var Array[T]; env: pointer) =
    if not x.data.isNil:
      for i in 0..<x.length: `=trace`(x.data[i], env)

  proc `=trace`*[L, T](x: var LengthEmbeddedArray[L, T]; env: pointer) =
    if not x.data.isNil:
      for a in x.mitems: `=trace`(a, env)

proc `=copy`*[T](a: var Array[T]; b: Array[T]) =
  if a.data == b.data: return
  `=destroy`(a)
  wasMoved(a)
  a.length = b.length
  if not b.data.isNil:
    when defined(js):
      {.emit: "`a`.data = Array(`b`.length);".}
    else:
      a.data = cast[ptr UncheckedArray[T]](alloc(b.length * sizeof(T)))
    for i in 0..<a.length:
      a.data[i] = b.data[i]

proc `=copy`*[L, T](a: var LengthEmbeddedArray[L, T]; b: LengthEmbeddedArray[L, T]) =
  if a.data == b.data: return
  `=destroy`(a)
  wasMoved(a)
  if not b.data.isNil:
    let len = b.len
    when defined(js):
      {.emit: "`a`.data = Array(1 + `len`);".}
    else:
      a.data = alloc(len * sizeof(T) + sizeof(L))
    cast[ptr UncheckedArray[L]](a)[0] = L(len)
    for i in 0 ..< len:
      a[i] = b[i]

proc `=sink`*[T](a: var Array[T]; b: Array[T]) =
  `=destroy`(a)
  wasMoved(a)
  a.length = b.length
  a.data = b.data

proc `=sink`*[L, T](a: var LengthEmbeddedArray[L, T]; b: LengthEmbeddedArray[L, T]) =
  `=destroy`(a)
  wasMoved(a)
  a.data = b.data

proc newArrayUninitialized*[T](length: int): Array[T] =
  result.length = length
  when defined(js):
    {.emit: "`result`.data = Array(`length`);".}
  else:
    result.data = cast[typeof(result.data)](alloc(result.length * sizeof(T)))

proc newLengthEmbeddedArrayUninitialized*[L, T](length: int): LengthEmbeddedArray[L, T] =
  rangeCheck length >= 0 and length <= high(L).int
  when defined(js):
    {.emit: "`result`.data = Array(`length` + 1);".}
  else:
    result.data = alloc(length * sizeof(T) + sizeof(L))
  cast[ptr UncheckedArray[L]](result.data)[0] = L(length)

proc newShortArrayUninitialized*[T](length: int): ShortArray[T] {.inline.} =
  newLengthEmbeddedArrayUninitialized[byte, T](length)

proc newArray*[T](length: int): Array[T] =
  result.length = length
  when defined(js):
    {.emit: "`result`.data = Array(`length`);".}
  else:
    result.data = cast[typeof(result.data)](alloc0(result.length * sizeof(T)))

proc newLengthEmbeddedArray*[L, T](length: int): LengthEmbeddedArray[L, T] =
  rangeCheck length >= 0 and length <= high(L).int
  when defined(js):
    {.emit: "`result`.data = Array(`length` + 1);".}
  else:
    result.data = alloc0(length * sizeof(T) + sizeof(L))
  cast[ptr UncheckedArray[L]](result.data)[0] = L(length)

template newShortArray*[T](length: int): ShortArray[T] =
  newLengthEmbeddedArray[byte, T](length)

proc toArray*[T](arr: openarray[T]): Array[T] =
  result = newArrayUninitialized[T](arr.len)
  for i in 0 ..< arr.len:
    result[i] = arr[i]

proc toLengthEmbeddedArray*[L, T](arr: openarray[T]): LengthEmbeddedArray[L, T] =
  result = newLengthEmbeddedArrayUninitialized[L, T](arr.len)
  for i in 0 ..< arr.len:
    result[i] = arr[i]

template toShortArray*[T](arr: openarray[T]): ShortArray[T] =
  toLengthEmbeddedArray[byte, T](arr)

template toOpenArray*[T](x: Array[T], first, last: int): auto =
  x.data.toOpenArray(first, last)

template toOpenArray*[L, T](x: LengthEmbeddedArray[L, T], first, last: int): auto =
  cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L)).toOpenArray(first, last)

proc `$`*(x: Array | LengthEmbeddedArray): string =
  result = "("
  var firstElement = true
  for value in items(x):
    if firstElement:
      firstElement = false
    else:
      result.add(", ")

    when value isnot string and value isnot seq and compiles(value.isNil):
      # this branch should not be necessary
      if value.isNil:
        result.add "nil"
      else:
        result.addQuoted(value)
    else:
      result.addQuoted(value)
  result.add(")")

proc `==`*[T](a, b: Array[T]): bool =
  let len = a.len
  if len != b.len: return false
  for i in 0 ..< len:
    if a[i] != b[i]: return false
  true

proc `==`*[L, T](a, b: LengthEmbeddedArray[L, T]): bool =
  let len = a.len
  if len != b.len: return false
  for i in 0 ..< len:
    if a[i] != b[i]: return false
  true

import hashes

proc hash*[T](a: Array[T]): Hash =
  mixin hash
  result = result !& hash a.len
  for i in 0 ..< a.len:
    result = result !& hash a[i]
  result = !$ result

proc hash*[L, T](a: LengthEmbeddedArray[L, T]): Hash =
  mixin hash
  result = result !& hash a.len
  for i in 0 ..< a.len:
    result = result !& hash a[i]
  result = !$ result

when isMainModule:
  block:
    var a = @[1, 2, 3, 4, 5].toArray()
    doAssert $a == "(1, 2, 3, 4, 5)"
    a[2] = 7
    doAssert $a == "(1, 2, 7, 4, 5)"

  block:
    var a = @[1, 2, 3, 4, 5].toShortArray()
    doAssert $a == "(1, 2, 3, 4, 5)"
    a[2] = 7
    doAssert $a == "(1, 2, 7, 4, 5)"

  block:
    var a = @[1, 2, 3, 4].toShortArray()
    doAssert $a == "(1, 2, 3, 4)"
    a[2] = 7
    doAssert $a == "(1, 2, 7, 4)"
