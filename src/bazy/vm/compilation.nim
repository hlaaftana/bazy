import primitives, language/expressions, std/[tables, sets]

type
  Variable* = ref object
    name*: string
    `type`*: Type
    staticValue*: Value
    stackIndex*: int

  Context* {.acyclic.} = ref object
    ## current module or function
    imports*: seq[Context]
    stackSize*: int
  
  Scope* {.acyclic.} = ref object
    ## restricted subset of variables in a context
    imports*: seq[Scope]
    parent*: Scope
    context*: Context
    variables*: seq[Variable]
    accumStackSize*: int
      ## the stack size from before the start of this scope

type TypeMatchKind* = enum
  # in order of strength
  tmNone, tmFalse, tmTrue, tmEqual

template boolMatch*(b: bool): TypeMatchKind =
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
    if matchType.kind != t.kind: return tmNone
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
        return rm
      let am = reduceMatch(matchType.arguments, t.arguments)
      min(am, rm)
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatchKind =
        if t1.len != t2.len: return low(TypeMatchKind)
        for k, v1 in t1:
          if k notin t2: return low(TypeMatchKind)
          let m = match(v1, t2[k])
          if m == low(TypeMatchKind): return m
          elif m < result: result = m
      tableMatch(matchType.fields, t.fields)
    of tyType:
      match(matchType.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmNone # unreachable
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
    boolMatch t.kind == matchType.baseKind
  of tyCustomMatcher:
    boolMatch matchType.typeMatcher(t)
  of tyContainingProperty:
    boolMatch matchType.containingProperty in t.properties
  of tyNot:
    const mapping = [
      tmNone: tmTrue,
      tmFalse: tmTrue,
      tmTrue: tmFalse,
      tmEqual: tmFalse]
    mapping[match(matchType.notType[], t)]

