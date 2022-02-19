import "."/[primitives, values], std/[tables, sets]

proc hasTag*(properties: Properties, property: PropertyTag): bool {.inline.} =
  properties.table.hasKey(property)

proc getArguments*(properties: Properties, property: PropertyTag): seq[Value] {.inline.} =
  properties.table[property]

iterator items*(properties: Properties): Property =
  for p, args in properties.table:
    yield Property(tag: p, arguments: args)

proc property*(tag: PropertyTag, args: varargs[Value]): Property {.inline.} =
  assert tag.argumentTypes.len == args.len, "argument length has to match"
  Property(tag: tag, arguments: @args)

proc property*(prop: Property): Property {.inline.} = prop

proc properties*(ps: varargs[Property, property]): Properties =
  result.table = initTable[PropertyTag, seq[Value]](ps.len)
  for p in ps:
    result.table[p.tag] = p.arguments

proc withProperties*(ty: sink Type, ps: varargs[Property, property]): Type {.inline.} =
  ty.properties = properties(ps)
  ty

proc tupleType*(s: varargs[Type]): Type =
  Type(kind: tyTuple, elements: @s)

proc funcType*(returnType: Type, arguments: varargs[Type]): Type {.inline.} =
  Type(kind: tyFunction, returnType: toRef(returnType), arguments: toRef(tupleType(arguments)))

proc tupleTypeWithVarargs*(s: varargs[Type], varargs: Type): Type =
  Type(kind: tyTuple, elements: @s, varargs: toRef(varargs))

proc funcTypeWithVarargs*(returnType: Type, arguments: varargs[Type], varargs: Type): Type {.inline.} =
  Type(kind: tyFunction, returnType: toRef(returnType), arguments: toRef(tupleTypeWithVarargs(arguments, varargs)))

proc union*(s: varargs[Type]): Type =
  Type(kind: tyUnion, operands: toRef(@s))

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyNoneValue..tyType}
  typeclassTypeKinds* = {tyAny..tyWithProperty}
  matcherTypeKinds* = typeclassTypeKinds + {tyCustomMatcher}
  atomicTypes* = {tyNoneValue, tyInteger, tyUnsigned, tyFloat, tyBoolean,
    tyString, tyExpression, tyStatement, tyScope}
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc hasNth*(t: Type, i: int): bool {.inline.} =
  assert t.kind == tyTuple
  i < t.elements.len or not t.varargs.isNil

proc nth*(t: Type, i: int): Type {.inline.} =
  assert t.kind == tyTuple
  if i < t.elements.len or t.varargs.isNil:
    t.elements[i]
  else:
    t.varargs[]

proc param*(t: Type, i: int): Type {.inline.} =
  assert t.kind == tyFunction
  t.arguments[].nth(i)

proc matches*(tm: TypeMatch): bool {.inline.} =
  tm >= lowestMatching

proc boolMatch*(b: bool): TypeMatch {.inline.} =
  if b: tmTrue else: tmFalse

template min*(a, b: TypeMatch): TypeMatch =
  let am = a
  (if am == tmNone: am
  else: system.min(am, b))

proc `+`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Covariant)
proc `-`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Contravariant)
proc `~`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Invariant)
proc `*`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Ultravariant)
proc `*`*(t: Type, variance: Variance): TypeBound {.inline.} = TypeBound(boundType: t, variance: variance)

proc converse*(tm: TypeMatch): TypeMatch =
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
      result = converse t.match(b.boundType)
  of Contravariant:
    result = t.match(b.boundType)
    if result == tmUnknown:
      result = converse b.boundType.match(t)
  of Invariant:
    result = min(b.boundType.match(t), converse t.match(b.boundType))
  of Ultravariant:
    result = b.boundType.match(t)
    if result != tmNone:
      result = max(result, converse t.match(b.boundType))

proc matchBound*(b: TypeBound, t: Type): bool {.inline.} =
  b.match(t).matches

proc match*(matcher, t: Type): TypeMatch =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  # properties do not have effect on default types besides dropping equal to almost equal
  if matcher == t: return tmEqual
  result = case matcher.kind
  of concreteTypeKinds:
    if matcher.kind != t.kind:
      return case t.kind
      of concreteTypeKinds:
        tmNone
      else:
        tmUnknown
    case matcher.kind
    of atomicTypes * concreteTypeKinds:
      tmAlmostEqual
    of tyReference, tyList, tySet:
      match(+matcher.elementType[], t.elementType[])
    of tyTuple:
      if matcher.elements.len != t.elements.len and matcher.varargs.isNil and t.varargs.isNil:
        return tmNone
      var max = t.elements.len
      if matcher.elements.len > t.elements.len and (max = matcher.elements.len; t.varargs.isNil):
        return tmNone
      var res = tmAlmostEqual
      for i in 0 ..< max:
        let m = match(+matcher.nth(i), t.nth(i))
        if m < res: res = m
        if res <= tmNone: return res
      if not matcher.varargs.isNil and not t.varargs.isNil:
        let vm = match(+matcher.varargs[], t.varargs[])
        if vm < res: res = vm
      res
    of tyFunction:
      min(
        match(-matcher.returnType[], t.returnType[]),
        match(+matcher.arguments[], t.arguments[]))
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
          if m <= tmNone: return m
          elif m < result: result = m
      tableMatch(matcher.fields, t.fields)
    of tyType:
      match(+matcher.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmUnknown # unreachable
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
    if matcher.typeMatcher.isNil:
      tmNone
    else:
      matcher.typeMatcher(t)
  of tyWithProperty:
    min(
      if not t.properties.hasTag(matcher.withProperty): tmFiniteFalse else: tmAlmostEqual,
      match(+matcher.typeWithProperty[], t))
  result = min(result, tmAlmostEqual)
  if result.matches:
    for p, args in matcher.properties.table:
      if not p.typeMatcher.isNil:
        result = min(result, p.typeMatcher(t, args))
        if result <= tmNone: return result

proc compare*(m1, m2: TypeMatch): int {.inline.} =
  ord(m1) - ord(m2)

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
  let cmp = compare(m1, m2)
  if cmp < 0:
    a
  elif cmp > 0:
    b
  elif m1 == tmEqual:
    a
  else:
    union(a, b)

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
    let ts = types; var yes = true; var i = 0
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
  result = case t.kind
  of tyNoneValue: value.kind == vkNone
  of tyInteger: value.kind == vkInteger
  of tyUnsigned: value.kind == vkUnsigned
  of tyFloat: value.kind == vkFloat
  of tyBoolean: value.kind == vkBoolean
  of tyFunction:
    # XXX (2) no information about signature
    value.kind in {vkFunction, vkNativeFunction}
  of tyTuple:
    value.kind == vkArray and value.tupleValue.unref.eachAre(t.elements)
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
    value.checkType(t.typeWithProperty[]) and value.toType.properties.hasTag(t.withProperty)
  of tyCustomMatcher: not t.valueMatcher.isNil and t.valueMatcher(value)
  if result:
    for p, args in t.properties.table:
      if not p.valueMatcher.isNil:
        result = result and p.valueMatcher(value, args)
        if not result: return result
