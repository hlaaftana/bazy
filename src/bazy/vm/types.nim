import "."/[primitives, values], std/[tables, sets]

proc `$`*(t: Type): string =
  case t.kind
  of tyNoneValue: "NoneValue"
  of tyInteger: "Int"
  of tyUnsigned: "Unsigned"
  of tyFloat: "Float"
  of tyBoolean: "Boolean"
  of tyString: "String"
  of tyExpression: "Expression"
  of tyStatement: "Statement"
  of tyScope: "Scope"
  of tyAny: "Any"
  of tyNone: "None"
  of tyFunction:
    "Function" & $(t.returnType[], t.arguments[]) 
  of tyTuple: "Tuple " & $t.elements[]
  of tyReference: "Reference " & $t.elementType[]
  of tyList: "List " & $t.elementType[]
  of tySet: "Set " & $t.elementType[]
  of tyTable: "Table" & $(t.keyType[], t.valueType[])
  of tyComposite: "Composite" & $t.fields
  of tyUnique: "Unique " & t.name & " " & $t.uniqueType.value
  of tyType: "Type " & $t.typeValue[]
  of tyUnion: "Union " & $t.operands[]
  of tyIntersection: "Intersection" & $t.operands[]
  of tyNot: "Not " & $t.notType[]
  of tyBaseType: "BaseType " & $t.baseKind
  of tyWithProperty: "WithProperty" & $(t.typeWithProperty[], t.withProperty)
  of tyCustomMatcher: "Custom"

proc paramType*(t: Type, i: int): Type =
  assert t.kind == tyFunction
  t.arguments[i]

type
  TypeMatch* = enum
    # in order of strength
    tmUnknown, tmNone, tmFiniteFalse, tmFalse, tmTrue, tmFiniteTrue, tmAlmostEqual, tmEqual

  Variance* = enum
    Covariant
    Contravariant
  
  TypeBound* = object
    boundType*: Type
    least*: TypeMatch
    variance*: Variance

const
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc matches*(tm: TypeMatch): bool {.inline.} =
  tm >= lowestMatching

proc boolMatch*(b: bool): TypeMatch {.inline.} =
  if b: tmTrue else: tmFalse

template min*(a, b: TypeMatch): TypeMatch =
  let am = a
  (if am == tmNone: am
  else: system.min(am, b))

proc `+`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, least: lowestMatching, variance: Covariant)
proc `-`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, least: lowestMatching, variance: Contravariant)
proc `*`*(t: Type, variance: Variance): TypeBound {.inline.} = TypeBound(boundType: t, least: lowestMatching, variance: variance)

proc `-`*(tm: TypeMatch): TypeMatch =
  case tm
  of tmEqual, tmNone, tmAlmostEqual, tmUnknown: tm
  of tmTrue: tmFalse
  of tmFalse: tmTrue
  of tmFiniteTrue: tmFiniteFalse
  of tmFiniteFalse: tmFiniteTrue

proc match*(matcher, t: Type): TypeMatch

proc match*(b: TypeBound, t: Type): TypeMatch =
  case b.variance
  of Covariant:
    result = b.boundType.match(t)
    if result == tmUnknown:
      result = -t.match(b.boundType)
  of Contravariant:
    result = t.match(b.boundType)
    if result == tmUnknown:
      result = -b.boundType.match(t)

proc matchBound*(b: TypeBound, t: Type): bool =
  when false:
    case b.variance
    of lowestMatching .. high(TypeMatch):
      rel >= b.variance
    of tmNone:
      rel == tmNone
    else:
      rel in {tmEqual, tmAlmostEqual} or (rel != tmNone and rel <= b.variance)
  b.match(t) >= b.least

proc match*(matcher, t: Type): TypeMatch =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  # properties do not have effect on default types besides dropping equal to almost equal
  proc reduceMatch(s1, s2: seq[Type]): TypeMatch =
    if s1.len != s2.len: return tmNone
    result = tmEqual
    for i in 0 ..< s1.len:
      let m = match(+s1[i], s2[i])
      if m == tmNone: return m
      elif m < result: result = m
  if matcher == t: return tmEqual
  result = case matcher.kind
  of concreteTypeKinds:
    if matcher.kind != t.kind:
      return case t.kind
      of concreteTypeKinds:
        tmNone
      of tyAny:#typeclassTypeKinds + matcherTypeKinds:
        tmFalse
      else:
        tmUnknown
    case matcher.kind
    of atomicTypes * concreteTypeKinds:
      tmAlmostEqual
    of tyReference, tyList, tySet:
      match(+matcher.elementType[], t.elementType[])
    of tyTuple:
      reduceMatch(matcher.elements[], t.elements[])
    of tyFunction:
      #echo (matcher, t, rm, am)
      min(
        match(-matcher.returnType[], t.returnType[]),
        reduceMatch(matcher.arguments[], t.arguments[]))
    of tyTable:
      min(
        match(+matcher.keyType[], t.keyType[]),
        match(+matcher.valueType[], t.valueType[]))
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatch =
        result = tmEqual
        if t1.len != t2.len: return tmNone
        for k, v1 in t1:
          if k notin t2: return tmNone
          let m = match(+v1, t2[k])
          if m == tmNone: return m
          elif m < result: result = m
      tableMatch(matcher.fields, t.fields)
    of tyUnique:
      tmNone
    of tyType:
      match(+matcher.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmNone # unreachable
  of tyAny: tmTrue
  of tyNone: tmUnknown
  of tyUnion:
    var max = tmFiniteFalse
    for a in matcher.operands[]:
      let m = match(+a, t)
      if m > max: max = m
      if max >= tmFiniteTrue:
        max = tmFiniteTrue
        break
    max
  of tyIntersection:
    var min = tmFiniteTrue
    for a in matcher.operands[]:
      let m = match(+a, t)
      if m < min: min = m
      if min <= tmFiniteFalse:
        min = tmFiniteFalse
        break
    min
  of tyNot:
    boolMatch not match(matcher.notType[], t).matches
  of tyBaseType:
    boolMatch t.kind == matcher.baseKind
  of tyCustomMatcher:
    boolMatch matcher.typeMatcher(t)
  of tyWithProperty:
    min(
      if matcher.withProperty notin t.properties: tmFiniteFalse else: tmAlmostEqual,
      match(+matcher.typeWithProperty[], t))
  result = min(result, tmAlmostEqual)

proc compare*(m1, m2: TypeMatch): int =
  if m1 == tmUnknown:
    result = ord(m2)
  else:
    result = ord(m1) - ord(m2)

proc compare*(t1, t2: Type): int =
  ## t1 < t2 mirrors being a subtype
  let
    m1 = t1.match(t2)
    m2 = t2.match(t1)
  assert not (m1 == tmEqual and m1 != m2), "equal match must be commutative"
  compare(m1, m2)

proc `<`*(a, b: Type): bool {.inline.} = compare(a, b) < 0
proc `<=`*(a, b: Type): bool {.inline.} = compare(a, b) <= 0
proc `>`*(a, b: Type): bool {.inline.} = compare(a, b) > 0
proc `>=`*(a, b: Type): bool {.inline.} = compare(a, b) >= 0

proc commonType*(a, b: Type): Type =
  let
    m1 = a.match(b)
    m2 = b.match(a)
  let cmp = ord(m1) - ord(m2)
  if cmp < 0:
    a
  elif cmp > 0:
    b
  elif m1 == tmEqual:
    a
  else:
    Type(kind: tyUnion, operands: toRef(@[a, b]))

import arrays

proc checkType*(value: Value, t: Type): bool =
  template eachAre(iter; types: seq[Type]): untyped =
    let ts = types; var yes = true; var i = 0
    for it in iter:
      if (i >= ts.len) or (not checkType(it, ts[i])):
        yes = false; break
      inc i
    yes and i == types.len
  template eachAre(iter; typ: Type): untyped =
    let ty = typ; var yes = true
    for it in iter:
      if not checkType(it, ty):
        yes = false; break
    yes
  template eachAreTable[T](iter; types: Table[T, Type]): untyped =
    let ts = types; var yes = true;var i = 0
    for key, value in iter:
      if (i >= ts.len) or (not checkType(value, ts[key])):
        yes = false; break
      inc i
    yes and i == types.len
  template eachAreTable(iter; kty, vty: Type): untyped =
    let kt = kty; let vt = vty; var yes = true
    for key, value in iter:
      if not checkType(key, kt) or not checkType(value, vt):
        yes = false; break
    yes
  case t.kind
  of tyNoneValue: value.kind == vkNone
  of tyInteger: value.kind == vkInteger
  of tyUnsigned: value.kind == vkUnsigned
  of tyFloat: value.kind == vkFloat
  of tyBoolean: value.kind == vkBoolean
  of tyFunction:
    # XXX no information about signature
    value.kind in {vkFunction, vkNativeFunction}
  of tyTuple:
    value.kind == vkTuple and value.tupleValue[].eachAre(t.elements[])
  of tyReference:
    value.kind == vkReference and (value.referenceValue.isNil or
      value.referenceValue[].checkType(t.elementType[]))
  of tyList:
    value.kind == vkList and value.listValue[].eachAre(t.elementType[])
  of tyString: value.kind == vkString
  of tySet:
    value.kind == vkSet and value.setValue[].eachAre(t.elementType[])
  of tyTable:
    value.kind == vkTable and value.tableValue[].eachAreTable(t.keyType[], t.valueType[])
  of tyExpression: value.kind == vkExpression
  of tyStatement: value.kind == vkStatement
  of tyScope: value.kind == vkScope
  of tyComposite:
    value.kind == vkComposite and value.compositeValue[].eachAreTable(t.fields)
  of tyUnique: true
  of tyType: value.kind == vkType and t.typeValue[].match(value.typeValue[]).matches
  of tyAny: true
  of tyNone: false
  of tyUnion:
    var res = false
    for ty in t.operands[]:
      if value.checkType(ty):
        res = true
        break
    res
  of tyIntersection:
    var res = true
    for ty in t.operands[]:
      if not value.checkType(ty):
        res = false
        break
    res
  of tyNot: not value.checkType(t.notType[])
  of tyBaseType: value.toType.kind == t.baseKind # XXX toType here is expensive
  of tyWithProperty:
    value.checkType(t.typeWithProperty[]) and t.withProperty in value.toType.properties
  of tyCustomMatcher: t.valueMatcher(value)
