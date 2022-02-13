type SafeArray*[T] = distinct seq[T]

proc newSafeArray*[T](len: int): SafeArray[T] =
  SafeArray[T](newSeq[T](len))

template toSafeArray*[T](x: openarray[T]): SafeArray[T] =
  SafeArray[T](@x)
template len*[T](arr: SafeArray[T]): int =
  seq[T](arr).len

template `[]`*[T](arr: SafeArray[T], index): untyped =
  `[]`(seq[T](arr), index)

template `[]=`*[T](arr: SafeArray[T], index, value) =
  `[]=`(seq[T](arr), index, value)

template hash*[T](arr: SafeArray[T]): untyped =
  mixin hash
  hash(seq[T](arr))

template `==`*[T](a, b: SafeArray[T]): untyped =
  mixin `==`
  `==`(seq[T](a), seq[T](b))

converter toSeq*[T](arr: SafeArray[T]): seq[T] = seq[T](arr)

when not defined(js):
  # XXX the array pointers are being treated as a ref counter and being incremented???
  type
    Array*[T] = object
      length*: int
      data*: ptr UncheckedArray[T]

    LengthEmbeddedArrayObj*[L, T] = object
      length: L
      data: UncheckedArray[T]

    LengthEmbeddedArray*[L, T] = ptr LengthEmbeddedArrayObj[L, T]

    ShortArray*[T] = LengthEmbeddedArray[byte, T]

  proc `=destroy`*[T](x: var Array[T]) =
    if not x.data.isNil:
      echo x.length
      echo typeof(x.data[0])
      for i in 0 ..< x.length:
        `=destroy`(x.data[i])
      dealloc(x.data)
      x.data = nil

  proc `=destroy`*[L, T](x: var LengthEmbeddedArrayObj[L, T]) =
    if not x.data.isNil:
      #let len = int(cast[ptr UncheckedArray[L]](x.data)[0])
      #for i in 0 ..< len:
      #  `=destroy`(cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L).uint)[i])
      for i in 0 ..< x.length:
        `=destroy`(x.data[i])
      dealloc(x.data)
      x.data = nil

  proc `=trace`*[T](x: var Array[T]; env: pointer) =
    if not x.data.isNil:
      for i in 0..<x.length: `=trace`(x.data[i], env)

  proc `=trace`*[L, T](x: var LengthEmbeddedArrayObj[L, T]; env: pointer) =
    if not x.data.isNil:
      let len = int(cast[ptr UncheckedArray[L]](x.data)[0])
      for i in 0 ..< len:
        `=trace`(cast[ptr UncheckedArray[T]](cast[uint](x.data) + sizeof(L).uint)[i], env)

  proc `=sink`*[T](a: var Array[T]; b: Array[T]) =
    `=destroy`(a)
    wasMoved(a)
    a.length = b.length
    a.data = b.data

  proc `=sink`*[L, T](a: var LengthEmbeddedArrayObj[L, T]; b: LengthEmbeddedArrayObj[L, T]) =
    `=destroy`(a)
    wasMoved(a)
    a.length = b.length
    a.data = b.data

  proc `=copy`*[T](a: var Array[T]; b: Array[T]) =
    when true:
      if a.data == b.data: return
      `=destroy`(a)
      wasMoved(a)
      a.length = b.length
      if not b.data.isNil:
        a.data = cast[ptr UncheckedArray[T]](alloc(a.length * sizeof(T)))
        for i in 0 ..< a.length:
          `=copy`(a.data[i], b.data[i])
    else:
      `=sink`(a, b)

  proc `=copy`*[L, T](a: var LengthEmbeddedArrayObj[L, T]; b: LengthEmbeddedArrayObj[L, T]) =
    when true:
      if a.data == b.data: return
      `=destroy`(a)
      wasMoved(a)
      if not b.data.isNil:
        let len = b.len
        a.data = alloc(len * sizeof(T) + sizeof(L))
        cast[ptr UncheckedArray[L]](a)[0] = L(len)
        for i in 0 ..< len:
          `=copy`(cast[ptr UncheckedArray[T]](cast[uint](a.data) + sizeof(L).uint)[i],
            cast[ptr UncheckedArray[T]](cast[uint](b.data) + sizeof(L).uint)[i])
    else:
      `=sink`(a, b)

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

  proc newArrayUninitialized*[T](length: int): Array[T] =
    result.length = length
    result.data = cast[typeof(result.data)](alloc(result.length * sizeof(T)))

  proc newLengthEmbeddedArrayUninitialized*[L, T](length: int): LengthEmbeddedArray[L, T] =
    rangeCheck length >= 0 and length <= high(L).int
    #result.data = alloc(length * sizeof(T) + sizeof(L))
    #cast[ptr UncheckedArray[L]](result.data)[0] = L(length)
    result = cast[typeof(result)](alloc(sizeof(L) + length * sizeof(T)))
    result.length = L(length)

  proc newShortArrayUninitialized*[T](length: int): ShortArray[T] {.inline.} =
    newLengthEmbeddedArrayUninitialized[byte, T](length)

  proc newArray*[T](length: int): Array[T] =
    result.length = length
    result.data = cast[typeof(result.data)](alloc0(result.length * sizeof(T)))

  proc newLengthEmbeddedArray*[L, T](length: int): LengthEmbeddedArray[L, T] =
    rangeCheck length >= 0 and length <= high(L).int
    result = cast[typeof(result)](alloc0(sizeof(L) + length * sizeof(T)))
    result.length = L(length)

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
    x.data.toOpenArray(first, last)

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
else:
  type
    Array*[T] = distinct seq[T]
    ShortArray*[T] = distinct seq[T]

  template newArray*[T](length: int): Array[T] =
    Array[T](newSeq[T](length))

  template newShortArray*[T](length: int): ShortArray[T] =
    ShortArray[T](newSeq[T](length))

  template toArray*[T](arr: openarray[T]): Array[T] =
    Array[T](@arr)

  template toShortArray*[T](arr: openarray[T]): ShortArray[T] =
    ShortArray[T](@arr)
  
  template len*[T](arr: Array[T]): int =
    seq[T](arr).len
  
  template len*[T](arr: ShortArray[T]): int =
    seq[T](arr).len
  
  template `[]`*[T](arr: Array[T], index): untyped =
    `[]`(seq[T](arr), index)
  
  template `[]`*[T](arr: ShortArray[T], index): untyped =
    `[]`(seq[T](arr), index)
  
  template `[]=`*[T](arr: Array[T], index, value) =
    `[]=`(seq[T](arr), index, value)
  
  template `[]=`*[T](arr: ShortArray[T], index, value) =
    `[]=`(seq[T](arr), index, value)
  
  template hash*[T](arr: Array[T]): untyped =
    mixin hash
    hash(seq[T](arr))
  
  template hash*[T](arr: ShortArray[T]): untyped =
    mixin hash
    hash(seq[T](arr))
  
  template `==`*[T](a, b: Array[T]): untyped =
    mixin `==`
    `==`(seq[T](a), seq[T](b))
  
  template `==`*[T](a, b: ShortArray[T]): untyped =
    mixin `==`
    `==`(seq[T](a), seq[T](b))
  
  converter toSeq*[T](arr: Array[T]): seq[T] = seq[T](arr)
  converter toSeq*[T](arr: ShortArray[T]): seq[T] = seq[T](arr)
