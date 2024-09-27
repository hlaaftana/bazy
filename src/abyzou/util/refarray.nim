# XXX move to library

type
  ArrayObj[T] {.byref.} = object
    length: int
    data: UncheckedArray[T]
  Array*[T] = object
    impl: ref ArrayObj[T]

template uninitArr*(arr, L): untyped =
  unsafeNew(arr.impl, sizeof(arr.impl.length) + L * sizeof(T))
  arr.impl.length = L

when defined(nimPreviewNonVarDestructor):
  # needs `ArrayObj` to be `byref`, or at least the parameter
  proc `=destroy`*[T](arr: ArrayObj[T]) =
    for i in 0 ..< arr.length:
      {.cast(raises: []).}:
        `=destroy`(arr.data[i])
else:
  {.warning[Deprecated]: off.}
  proc `=destroy`*[T](arr: var ArrayObj[T]) =
    for i in 0 ..< arr.length:
      {.cast(raises: []).}:
        `=destroy`(arr.data[i])

proc `=wasMoved`*[T](arr: var ArrayObj[T]) {.inline.} =
  arr.length = 0

proc `=trace`*[T](arr: var ArrayObj[T]; env: pointer) =
  for i in 0 ..< arr.length:
    `=trace`(arr.data[i], env)

proc `=trace`*[T](arr: var Array[T]; env: pointer) =
  if not arr.impl.isNil:
    for i in 0 ..< arr.impl.length:
      `=trace`(arr.impl.data[i], env)

proc len*[T](x: Array[T]): int {.inline.} =
  if x.impl.isNil: 0
  else: x.impl.length

proc `[]`*[T](x: Array[T], i: int): lent T {.inline.} =
  x.impl.data[i]

proc `[]`*[T](x: var Array[T], i: int): var T {.inline.} =
  x.impl.data[i]

proc `[]=`*[T](x: var Array[T], i: int, val: sink T) {.inline.} =
  x.impl.data[i] = val

iterator items*[T](x: Array[T]): T =
  let L = x.len
  for i in 0 ..< L:
    yield x.impl.data[i]

iterator mitems*[T](x: var Array[T]): var T =
  let L = x.len
  for i in 0 ..< L:
    yield x.impl.data[i]

iterator pairs*[T](x: Array[T]): (int, T) =
  let L = x.len
  for i in 0 ..< L:
    yield (i, x.impl.data[i])

iterator mpairs*[T](x: var Array[T]): (int, var T) =
  let L = x.len
  for i in 0 ..< L:
    yield (i, x.impl.data[i])

proc newArrayUninitialized*[T](length: int): Array[T] =
  uninitArr result, length

proc newArray*[T](length: int): Array[T] =
  uninitArr result, length
  for i in 0 ..< length:
    result.impl.data[i] = default(T)

proc toArray*[T](arr: openarray[T]): Array[T] =
  result = newArrayUninitialized[T](arr.len)
  for i in 0 ..< arr.len:
    result[i] = arr[i]

template toOpenArray*[T](x: Array[T], first, last: int): auto =
  toOpenArray(addr x.impl.data, first, last)

proc `$`*[T](x: Array[T]): string =
  result = "Array("
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

import hashes

proc hash*[T](a: Array[T]): Hash =
  mixin hash
  result = result !& hash a.len
  for i in 0 ..< a.len:
    result = result !& hash a[i]
  result = !$ result

type RefArray*[T] = distinct Array[T]

template toRefArray*[T](foo: Array[T]): RefArray[T] = RefArray[T](foo)
template toRefArray*(foo): RefArray = toRefArray(toArray(foo))
template unref*[T](arr: RefArray[T]): Array[T] = Array[T](arr)

proc `[]=`*[T](x: RefArray[T], i: int, val: sink T) {.inline.} =
  x.unref.impl.data[i] = val

proc `==`*[T](a, b: RefArray[T]): bool {.inline.} =
  `==`(a.unref, b.unref)

proc hash*[T](x: RefArray[T]): Hash =
  hash(x.unref)

when isMainModule:
  when true:
    type Foo = ref object
      x: int
    proc foo(x: int): Foo = Foo(x: x)
    proc `$`(f: Foo): string = $f.x
    var f: Array[Array[Foo]]
    block:
      var a = @[foo(1), foo(2), foo(3), foo(4), foo(5)].toArray()
      doAssert $a == "Array(1, 2, 3, 4, 5)"
      a[2] = foo 7
      doAssert $a == "Array(1, 2, 7, 4, 5)"
      f = [a].toArray
    block:
      echo f
  
  block:
    type Foo = object
      case atom: bool
      of false:
        node: Array[Foo]
      of true:
        leaf: int
    
    proc tree(arr: varargs[Foo]): Foo =
      Foo(atom: false, node: toArray(@arr))
    proc leaf(x: int): Foo = Foo(atom: true, leaf: x)
    let x = tree(leaf(1), tree(leaf(2), tree(leaf(3), tree(leaf(4), tree(leaf(5))))))
    echo x
