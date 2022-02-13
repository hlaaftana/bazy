import "."/[primitives, values], std/[tables, sets]

proc `$`*(t: Type): string =
  case t.kind
  of tyNoneValue: "NoneValue"
  of tyInteger: "Int"
  of tyUnsigned: "Unsigned"
  of tyFloat: "Float"
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

type
  TypeMatch* = enum
    # in order of strength
    tmNone, tmFalse, tmTrue, tmAlmostEqual, tmEqual
  
  TypeBound* = object
    boundType*: Type
    variance*: TypeMatch

const
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc matches*(tm: TypeMatch): bool {.inline.} =
  tm >= lowestMatching

proc boolMatch*(b: bool): TypeMatch {.inline.} =
  if b: tmTrue else: tmFalse

proc match*(matcher, t: Type): TypeMatch =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  # properties do not have effect on default types besides dropping equal to almost equal
  proc reduceMatch(s1, s2: seq[Type]): TypeMatch =
    if s1.len != s2.len: return low(TypeMatch)
    result = pred(tmEqual)
    for i in 0 ..< s1.len:
      let m = match(s1[i], s2[i])
      if m == low(TypeMatch): return m
      elif m < result: result = m
  if matcher == t: return tmEqual
  result = case matcher.kind
  of concreteTypeKinds:
    if matcher.kind != t.kind: return tmNone
    case matcher.kind
    of atomicTypes:
      tmAlmostEqual
    of tyReference, tyList, tySet:
      match(matcher.elementType[], t.elementType[])
    of tyTuple:
      reduceMatch(matcher.elements[], t.elements[])
    of tyFunction:
      let rm = match(matcher.returnType[], t.returnType[])
      #if rm in {tmNone, tmFalse}:
      #  return rm
      let am = reduceMatch(matcher.arguments[], t.arguments[])
      min(am, rm)
    of tyTable:
      min(
        match(matcher.keyType[], t.keyType[]),
        match(matcher.valueType[], t.valueType[]))
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatch =
        result = pred(tmEqual)
        if t1.len != t2.len: return low(TypeMatch)
        for k, v1 in t1:
          if k notin t2: return low(TypeMatch)
          let m = match(v1, t2[k])
          if m == low(TypeMatch): return m
          elif m < result: result = m
      tableMatch(matcher.fields, t.fields)
    of tyUnique:
      tmNone
    of tyType:
      match(matcher.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmNone # unreachable
  of tyAny: tmTrue
  of tyNone: tmFalse
  of tyUnion:
    for a in matcher.operands[]:
      if match(a, t).matches:
        return tmTrue
    tmFalse
  of tyIntersection:
    for a in matcher.operands[]:
      if not match(a, t).matches:
        return tmFalse
    tmTrue
  of tyNot:
    boolMatch not match(matcher.notType[], t).matches
  of tyBaseType:
    boolMatch t.kind == matcher.baseKind
  of tyCustomMatcher:
    boolMatch matcher.typeMatcher(t)
  of tyWithProperty:
    min(
      if matcher.withProperty notin t.properties: tmFalse else: pred(tmEqual),
      min(match(matcher.typeWithProperty[], t), pred(tmEqual)))

proc compare*(t1, t2: Type): int =
  ## t1 < t2 mirrors being a subtype
  let
    m1 = t1.match(t2)
    m2 = t2.match(t1)
  assert not (m1 == tmEqual and m1 != m2), "equal match must be commutative"
  result = ord(m1) - ord(m2)

proc `<`*(a, b: Type): bool {.inline.} = compare(a, b) < 0
proc `<=`*(a, b: Type): bool {.inline.} = compare(a, b) <= 0
proc `>`*(a, b: Type): bool {.inline.} = compare(a, b) > 0
proc `>=`*(a, b: Type): bool {.inline.} = compare(a, b) >= 0

proc `+`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: lowestMatching)
proc `-`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: highestNonMatching)
proc `*`*(t: Type, variance: TypeMatch): TypeBound {.inline.} = TypeBound(boundType: t, variance: variance)

proc matchBound*(b: TypeBound, t: Type): bool {.inline.} =
  let rel = b.boundType.match(t)
  case b.variance
  of lowestMatching .. high(TypeMatch):
    rel >= b.variance
  of tmNone:
    rel == tmNone
  else:
    rel in {tmEqual, tmAlmostEqual} or (rel != tmNone and rel <= b.variance)

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
  of tyFunction:
    # XXX no way to check argument types
    value.kind in {vkFunction, vkNativeFunction}
  of tyTuple:
    (value.kind == vkTuple and value.tupleValue[].eachAre(t.elements[])) or
      (value.kind == vkShortTuple and value.shortTupleValue.eachAre(t.elements[]))
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
  of tyBaseType: value.toType.kind == t.baseKind # XXX
  of tyWithProperty:
    value.checkType(t.typeWithProperty[]) and t.withProperty in value.toType.properties
  of tyCustomMatcher: t.valueMatcher(value)
