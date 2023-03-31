import hashes

type
  IdImpl = uint64

template id(kind) {.dirty.} =
  type `kind Id`* = distinct IdImpl
  proc `==`*(a, b: `kind Id`): bool {.borrow.}
  proc hash*(a: `kind Id`): Hash {.borrow.}
  # 0 reserved for none values
  # can change with something else later
  var `counter kind`: IdImpl = 0
  proc `new kind Id`*(): `kind Id` =
    inc `counter kind`
    `kind Id`(`counter kind`)

id(Property)
id(TypeParameter)

# XXX maybe intern strings?
