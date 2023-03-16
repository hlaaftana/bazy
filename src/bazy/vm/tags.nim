import tables, hashes

type
  TagImpl = uint64
  Tag* = distinct TagImpl

template impl(t: Tag): TagImpl = t.TagImpl
proc `==`*(a, b: Tag): bool = a.impl == b.impl

var
  symbols: Table[TagImpl, string]
    ## map of tag ids to strings
  counter: Table[TagImpl, int]
    ## number of consecutive filled entries per hash
  fakeTags: Table[TagImpl, TagImpl]
    ## tag ids that map to other tags

proc tag*(name: string): Tag =
  let h = cast[TagImpl](hash(name))
  var t = h
  var c = counter.getOrDefault(h, -1)
  if c < 0:
    counter[h] = 0
    c = 0
  #t += TagImpl(c)
  while true:
    inc counter[h]
    symbols.withValue(t, val) do:
      if val[] == name:
        return t.Tag
    do:
      if not fakeTags.hasKey(t):
        symbols[t] = name
        return t.Tag
    inc t

proc uniqueTag*(name: string): Tag =
  let h = cast[TagImpl](hash(name))
  let orig = tag(name).impl
  var t = h
  t += TagImpl(counter[h])
  fakeTags[t] = orig
  inc counter[h]
  t.Tag

proc toSymbol*(id: Tag): string =
  let real = fakeTags.getOrDefault(id.impl, id.impl)
  symbols[real]

proc `$`*(id: Tag): string = toSymbol(id)
