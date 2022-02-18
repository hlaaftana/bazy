import ../defines

when useArrays:
  import arrayimpl
  export arrayimpl
  when arraysEmbedLength:
    type ArrayRef*[T] = Array[T]
    template toArrayRef*(foo): ArrayRef = toArray(foo)
  else:
    type ArrayRef*[T] = ref Array[T]
    template toArrayRef*(foo): ArrayRef =
      var r: ref typeof(toArray(foo))
      new(r)
      r[] = toArray(foo)
      r
else:
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

  type ArrayRef*[T] = ref Array[T]
  template toArrayRef*(foo): ArrayRef =
    var res = new(typeof(toArray(foo)))
    res[] = toArray(foo)
    res
