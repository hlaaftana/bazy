import ".."/[primitives, compilation, values]

import common

module types:
  define "Type", Ty(Type)
  typedTempl "type_of", 1:
    let t = args[0].cachedType
    result = toValue constant(t, Type(kind: tyType, typeValue: toRef(t)))
  # todo: .call / .[] for type arguments
