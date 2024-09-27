import ../defines

when arrayImpl == "seq":
  template distinctSeq(name) {.dirty.} =
    type `name`*[T] = distinct seq[T]

    proc `new name`*[T](len: int): `name`[T] =
      `name`[T](newSeq[T](len))

    template `to name`*[T](x: openarray[T]): `name`[T] =
      `name`[T](@x)

    template toSeq*[T](arr: `name`[T]): seq[T] = seq[T](arr)
    converter toSeqConverter*[T](arr: `name`[T]): seq[T] = toSeq(arr)

    template len*[T](arr: `name`[T]): int =
      toSeq(arr).len

    template `[]`*[T](arr: `name`[T], index): untyped =
      `[]`(toSeq(arr), index)

    template `[]=`*[T](arr: `name`[T], index, value) =
      `[]=`(toSeq(arr), index, value)

    template hash*[T](arr: `name`[T]): untyped =
      mixin hash
      hash(toSeq(arr))

    template `==`*[T](a, b: `name`[T]): untyped =
      mixin `==`
      `==`(toSeq(a), toSeq(b))

  distinctSeq Array
elif arrayImpl == "hybrid":
  import ../disabled/hybridarrays
  export hybridarrays
elif arrayImpl == "ref":
  import ../util/refarray
  export refarray
elif arrayImpl == "manta":
  import manta/refarray
  export refarray
  type Array*[T] = RefArray[T]
  template toArray*[T](foo: openarray[T]): RefArray =
    toRefArray[T](foo)
  template newArray*[T](foo): RefArray[T] =
    newRefArray[T](foo)
  template unref*[T](arr: RefArray[T]): Array[T] =
    arr

when not declared(RefArray):
  type RefArray*[T] = ref Array[T]
  template toRefArray*(foo): RefArray =
    var res = new(typeof(toArray(foo)))
    res[] = toArray(foo)
    res

  proc `==`*[T](a, b: RefArray[T]): bool {.inline.} =
    mixin `==`
    system.`==`(a, b) or (not a.isNil and not b.isNil and a[] == b[])

  import hashes

  proc hash*[T](x: RefArray[T]): Hash =
    mixin hash
    if x.isNil:
      hash(pointer nil)
    else:
      hash(x[])
