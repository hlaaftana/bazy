type
  ArrayInfo[T] = ptr object
    length: int
    data: UncheckedArray[T]
  
  Array*[T] = object
    info: ArrayInfo[T]

template initarr*(arr, L, allocProc): untyped =
  arr.info = cast[typeof(arr.info)](allocProc(sizeof(arr.info.length) + L * sizeof(T)))
  arr.info.length = L

when false:
  # XXX very fragile, memory corruption with nesting of any kind
  proc `=destroy`*[T](arr: var Array[T]) =
    if not arr.info.isNil:
      let len = arr.info.length
      for i in 0 ..< len:
        `=destroy`(arr.info.data[i])
      dealloc(arr.info)
      arr.info = nil

proc `=copy`*[T](a: var Array[T], b: Array[T]) #[{.error.}]# =
  a.info = b.info

proc `=trace`[T](arr: var Array[T]; env: pointer) =
  if arr.info != nil:
    for i in 0 ..< arr.info.length:
      `=trace`(arr.info.data[i], env)

proc len*[T](x: Array[T]): int {.inline.} =
  if x.info.isNil:
    0
  else:
    x.info.length

proc `[]`*[T](x: Array[T], i: int): lent T {.inline.} =
  x.info.data[i]

proc `[]`*[T](x: var Array[T], i: int): var T {.inline.} =
  x.info.data[i]

proc `[]=`*[T](x: var Array[T], i: int, val: sink T) {.inline.} =
  x.info.data[i] = val

iterator items*[T](x: Array[T]): T =
  let L = x.len
  for i in 0 ..< L:
    yield x[i]

iterator mitems*[T](x: var Array[T]): var T =
  let L = x.len
  for i in 0 ..< L:
    yield x[i]

proc newArrayUninitialized*[T](length: int): Array[T] =
  initarr result, length, alloc

proc newArray*[T](length: int): Array[T] =
  initarr result, length, alloc0

proc toArray*[T](arr: sink openarray[T]): Array[T] =
  result = newArrayUninitialized[T](arr.len)
  for i in 0 ..< arr.len:
    result[i] = arr[i]

template toOpenArray*[T](x: Array[T], first, last: int): auto =
  (addr x.info.data).toOpenArray(first, last)

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

when isMainModule:
  when false:
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
  
  type Foo = object
    case atom: bool
    of false:
      node: Array[Foo]
    of true:
      leaf: int
  
  proc tree(arr: varargs[Foo]): Foo =
    Foo(atom: false, node: toArray(@arr))
  proc leaf(x: int): Foo = Foo(atom: true, leaf: x)
  echo tree(leaf(1), tree(leaf(2), tree(leaf(3))))#, tree(leaf(4), tree(leaf(5))))))
