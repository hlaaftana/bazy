type Box*[T] = object
  ## value type version of `ref T`
  inner*: ref T

#proc `=copy`*[T](a: var Box[T], b: Box[T]) =
#  if a.inner.isNil:
#    new(a.inner)
#  `=copy`(a.inner[], b.inner[])

proc isNil*[T](x: Box[T]): bool {.inline.} = x.inner.isNil

template box*(T: typedesc): typedesc = Box[T]
proc box*[T](x: sink T): Box[T] =
  new(result.inner)
  result.inner[] = x

proc unbox*[T](x: Box[T]): lent T {.inline.} = x.inner[]

proc shallowStore*[T](a: var Box[T], b: T) =
  if a.inner.isNil:
    new(a.inner)
  a.inner[] = b

proc store*[T](a: var Box[T], b: T) =
  if not a.inner.isNil:
    a.inner = nil
  new(a.inner)
  a.inner[] = b

proc `$`*[T](x: Box[T]): string {.inline.} =
  mixin `$`
  if x.isNil:
    "nil"
  else:
    $x.unbox

proc `==`*[T](a, b: Box[T]): bool {.inline.} =
  mixin `==`
  system.`==`(a.inner, b.inner) or (not a.isNil and not b.isNil and a.unbox == b.unbox)

import hashes

proc hash*[T](x: Box[T]): Hash =
  mixin hash
  if x.isNil:
    hash(nil)
  else:
    hash(x.unbox)
