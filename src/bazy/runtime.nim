import tables, sets, hashes

# type system should exist for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

{.push acyclic.}

type
  ValueKind* = enum
    vkNone
      ## some kind of null value
    vkInteger, vkUnsigned, vkFloat
      ## size is implementation detail
      ## same as pointer size for now but may be 32 or 48 bits
      ## if values use pointer packing
    vkFunction
    vkTuple
      ## like java array but typed like TS, not necessarily hetero or homogenously typed
      ## caps out at 255 size so it can be a single pointer that points to a length
      ## and then elements
    vkReference
      ## reference (pointer) to value
    vkUniqueReference
      ## reference to value identified by its address (singleton)
    vkString
      ## general byte sequence type
    vkSeq
      ## both seq and string are references to save memory
    vkComposite
      ## like tuple, but fields are tied to names and unordered
      ## (really the names define the order and the names can at least be shortstrings)
    vkPropertyReference
      ## references with some type properties attached at runtime
      ## which are also runtime values

  UniqueReference* = distinct ref Value

  Value* = object
    # entire thing can be pointer tagged, but would need GC hooks
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: int64
    of vkUnsigned:
      unsignedValue*: uint64
    of vkFloat:
      floatValue*: float
    of vkFunction:
      functionValue*: proc (args: sink seq[Value]): Value
    of vkTuple:
      # supposed to be just length and pointer, might do 16 bits length 48 bits pointer
      # or like shortstring
      tupleValue*: ref seq[Value]
    of vkReference:
      referenceValue*: ref Value
    of vkUniqueReference:
      uniqueReferenceValue*: UniqueReference
    of vkString:
      stringValue*: ref string
    of vkSeq:
      seqValue*: ref seq[Value]
    of vkComposite:
      # supposed to be represented more efficiently
      # short string instead of string
      # compare composite keys by hashing keys and caching it
      compositeValue*: ref Table[string, Value]
    of vkPropertyReference:
      propertyRefValue*: ref RuntimePropertyObj

  RuntimePropertyObj* = object
    properties*: HashSet[Value]
    value*: Value

  TypeKind* = enum
    # concrete
    tyNoneValue,
    tyInteger, tyUnsigned, tyFloat,
    tyFunction, tyTuple,
    tyReference,
    tyString, tySeq,
    tyComposite,
    # typeclass
    tyAny, tyNone, tyUnion, tyIntersection, tyNot,
    tyBaseType, tyWithProperty,
    # matcher
    tyCustomMatcher
  
  Type* = object
    properties*: HashSet[Value]
    case kind*: TypeKind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat,
      tyString, tyAny, tyNone:
      discard
    of tyFunction:
      arguments*: seq[Type]
      returnType*: ref Type
    of tyTuple:
      elements*: seq[Type]
    of tySeq, tyReference:
      elementType*: ref Type
    of tyComposite:
      fields*: Table[string, Type]
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: ref Type
    of tyBaseType:
      baseKind*: TypeKind
    of tyWithProperty:
      withProperty*: Value
    of tyCustomMatcher:
      matcher*: proc (t: Type): bool
      #compare*: proc (otherMatcher, t: Type): bool
        # if closer to `t` than `otherMatcher`, return true

{.pop.} # acyclic

include includes/runtimeequalsboilerplate

const
  concreteTypeKinds* = {tyNoneValue..tyComposite}
  typeclassTypeKinds* = {tyAny..tyWithProperty}
  matcherTypeKinds* = typeclassTypeKinds + {tyCustomMatcher}

type TypeMatchKind* = enum
  # in order of strength
  tmNone, tmFalse, tmTrue, tmEqual

template boolToMatch*(b: bool): TypeMatchKind =
  if b: tmTrue else: tmFalse

proc match*(matchType, t: Type): TypeMatchKind =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  proc reduceMatch(s1, s2: seq[Type]): TypeMatchKind =
    if s1.len != s2.len: return low(TypeMatchKind)
    result = high(TypeMatchKind)
    for i in 0 ..< s1.len:
      let m = match(s1[i], s2[i])
      if m == low(TypeMatchKind): return m
      elif m < result: result = m
  if matchType == t: return tmEqual
  result = case matchType.kind
  of concreteTypeKinds:
    if matchType.kind == t.kind:
      case matchType.kind
      of tyNoneValue, tyInteger, tyUnsigned, tyFloat, tyString:
        tmEqual
      of tyReference, tySeq:
        match(matchType.elementType[], t.elementType[])
      of tyTuple:
        reduceMatch(matchType.elements, t.elements)
      of tyFunction:
        let rm = match(matchType.returnType[], t.returnType[])
        if rm in {tmNone, tmFalse}:
          rm
        else:
          let am = reduceMatch(matchType.arguments, t.arguments)
          if am == tmEqual and rm == tmTrue:
            tmTrue
          else:
            am
      of tyComposite:
        proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatchKind =
          if t1.len != t2.len: return low(TypeMatchKind)
          for k, v1 in t1:
            if k notin t2: return low(TypeMatchKind)
            let m = match(v1, t2[k])
            if m == low(TypeMatchKind): return m
            elif m < result: result = m
        tableMatch(matchType.fields, t.fields)
      of matcherTypeKinds: tmNone
    else:
      tmNone
  of tyAny: tmTrue
  of tyNone: tmFalse
  of tyUnion:
    for a in matchType.operands:
      if match(a, t) >= tmTrue:
        return tmTrue
    tmFalse
  of tyIntersection:
    for a in matchType.operands:
      if match(a, t) <= tmFalse:
        return tmFalse
    tmTrue
  of tyBaseType:
    boolToMatch t.kind == matchType.baseKind
  of tyCustomMatcher:
    boolToMatch matchType.matcher(t)
  of tyWithProperty:
    boolToMatch matchType.withProperty in t.properties
  of tyNot:
    const mapping = [
      tmNone: tmTrue,
      tmFalse: tmTrue,
      tmTrue: tmFalse,
      tmEqual: tmFalse]
    mapping[match(matchType.notType[], t)]
