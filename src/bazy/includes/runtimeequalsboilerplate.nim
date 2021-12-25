# dumb nim forces boilerplate implementation

proc hash*(p: UniqueReference): Hash =
  result = result !& hash(cast[pointer]((ref Value)(p)))
  result = !$ result

proc hash(r: ref): Hash =
  result = result !& hash(r[])
  result = !$ result

proc hash*(v: Value): Hash =
  template mix(x) =
    result = result !& hash(x)
  defer: result = !$ result
  mix v.kind
  case v.kind
  of vkNone: discard
  of vkInteger:
    mix v.integerValue
  of vkUnsigned:
    mix v.unsignedValue
  of vkFloat:
    mix v.floatValue
  of vkFunction:
    mix v.functionValue
  of vkTuple:
    mix v.tupleValue
  of vkReference:
    mix v.referenceValue
  of vkString:
    mix v.stringValue
  of vkSeq:
    mix v.seqValue
  of vkComposite:
    mix v.compositeValue
  of vkUniqueReference:
    mix v.uniqueReferenceValue
  of vkPropertyReference:
    mix v.propertyRefValue

proc `==`*(a, b: Value): bool
proc `==`*(a, b: Type): bool
  
proc `==`*[T: Value | Type](a, b: ref T): bool =
  system.`==`(a, b) or (not a.isNil and not b.isNil and a[] == b[])

template `==`*(p1, p2: UniqueReference): bool =
  cast[pointer]((ref Value)(p1)) == cast[pointer]((ref Value)(p2))

proc `==`*(a, b: Value): bool =
  if a.kind != b.kind: return false
  case a.kind
  of vkNone: true
  of vkInteger:
    a.integerValue == b.integerValue
  of vkUnsigned:
    a.unsignedValue == b.unsignedValue
  of vkFloat:
    a.floatValue == b.floatValue
  of vkFunction:
    a.functionValue == b.functionValue
  of vkTuple:
    a.tupleValue == b.tupleValue
  of vkReference:
    a.referenceValue == b.referenceValue
  of vkString:
    a.stringValue == b.stringValue
  of vkSeq:
    a.seqValue == b.seqValue
  of vkComposite:
    a.compositeValue == b.compositeValue
  of vkUniqueReference:
    a.uniqueReferenceValue == b.uniqueReferenceValue
  of vkPropertyReference:
    a.propertyRefValue == b.propertyRefValue

proc `==`*(a, b: Type): bool =
  if a.kind != b.kind: return false
  if a.properties != b.properties: return false
  case a.kind:
  of tyNoneValue, tyInteger, tyUnsigned, tyFloat,
    tyString, tyAny, tyNone:
    true
  of tyFunction:
    a.arguments == b.arguments and
      a.returnType == b.returnType
  of tyTuple:
    a.elements == b.elements
  of tySeq, tyReference:
    a.elementType == b.elementType
  of tyComposite:
    a.fields == b.fields
  of tyUnion, tyIntersection:
    a.operands == b.operands
  of tyNot:
    a.notType == b.notType
  of tyBaseType:
    a.baseKind == b.baseKind
  of tyWithProperty:
    a.withProperty == b.withProperty
  of tyCustomMatcher:
    a.matcher == b.matcher
