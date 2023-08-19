import "."/[primitives, values, ids], std/[tables, sets]

type PropertyInstance* = object
  tag*: Property
  value*: Value

proc `$`*(p: PropertyInstance): string =
  result = $p.tag
  if p.value.kind != vkNone:
    result.add('(')
    result.add($p.value)
    result.add(')')

proc property*(tag: Property, arg: Value): PropertyInstance {.inline.} =
  PropertyInstance(tag: tag, value: arg)

proc property*(prop: PropertyInstance): PropertyInstance {.inline.} = prop

proc properties*(ps: varargs[PropertyInstance, property]): Table[Property, Value] =
  result = initTable[Property, Value](ps.len)
  for p in ps:
    result[p.tag] = p.value

proc withProperties*(ty: sink Type, ps: varargs[PropertyInstance, property]): Type {.inline.} =
  ty.properties = properties(ps)
  ty

type TypeError* = object of CatchableError

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyNoneValue..tyType}
  typeclassTypeKinds* = {tyAny..tyWithProperty}
  matcherTypeKinds* = typeclassTypeKinds + {tyCustomMatcher}
  atomicTypes* = {tyNoneValue,
    tyInt32, tyUint32, tyFloat32, tyBool,
    tyInt64, tyUint64, tyFloat64,
    tyString, tyExpression, tyStatement, tyScope}
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc tupleType*(s: varargs[Type]): Type =
  Type(kind: tyTuple, elements: @s)

proc funcType*(returnType: Type, arguments: varargs[Type]): Type {.inline.} =
  Type(kind: tyFunction, returnType: returnType.box, arguments: tupleType(arguments).box)

proc tupleTypeWithVarargs*(s: varargs[Type], varargs: Type): Type =
  Type(kind: tyTuple, elements: @s, varargs: varargs.box)

proc funcTypeWithVarargs*(returnType: Type, arguments: varargs[Type], varargs: Type): Type {.inline.} =
  Type(kind: tyFunction, returnType: returnType.box, arguments: tupleTypeWithVarargs(arguments, varargs).box)

proc union*(s: varargs[Type]): Type =
  Type(kind: tyUnion, operands: @s)

const definiteTypeLengths*: array[TypeKind, int] = [
  tyNone: 0,
  tyNoneValue: 0,
  tyInt32: 0,
  tyUint32: 0,
  tyFloat32: 0,
  tyBool: 0,
  tyInt64: 0,
  tyUint64: 0,
  tyFloat64: 0,
  tyReference: 1,
  tyTuple: -1,
  tyFunction: 2,
  tyList: 1,
  tyString: 0,
  tySet: 1,
  tyTable: 2,
  tyExpression: 0,
  tyStatement: 0,
  tyScope: 0,
  tyType: 1,
  tyAny: 0,
  tyUnion: -1,
  tyIntersection: -1,
  tyNot: 1,
  tyBaseType: -1,
  tyWithProperty: -1,
  tyCustomMatcher: 0,
  tyParameter: -1,
  #tyGeneric: -1
]

proc len*(t: Type): int =
  result = definiteTypeLengths[t.kind]
  if result < 0:
    case t.kind
    of tyTuple:
      if t.varargs.isNone:
        result = t.elements.len + t.unorderedFields.len
    of tyUnion, tyIntersection:
      result = t.operands.len
    else: discard

proc hasNth*(t: Type, i: int): bool {.inline.} =
  i < t.len or (t.kind == tyTuple and not t.varargs.isNone)

proc nth*(t: Type, i: int): Type =
  case t.kind
  of tyNoneValue,
    tyInt32, tyUint32, tyFloat32, tyBool,
    tyInt64, tyUint64, tyFloat64,
    tyString, tyExpression, tyStatement, tyScope,
    tyAny, tyNone:
    discard # inapplicable
  of tyFunction:
    if i == 0:
      result = t.arguments.unbox
    else:
      result = t.returnType.unbox
  of tyTuple:
    if i < t.elements.len or t.varargs.isNone:
      result = t.elements[i]
    else:
      result = t.varargs.unbox
  of tyReference, tyList, tySet:
    result = t.elementType.unbox
  of tyTable:
    if i == 0:
      result = t.keyType.unbox
    else:
      result = t.valueType.unbox
  of tyType:
    result = t.typeValue.unbox
  of tyUnion, tyIntersection:
    # this is actually not supposed to happen
    result = t.operands[i]
  of tyNot:
    result = t.notType.unbox
  of tyBaseType:
    discard # inapplicable
  of tyWithProperty:
    discard # inapplicable
  of tyCustomMatcher:
    discard # inapplicable
  of tyParameter:#, tyGeneric:
    discard # what

proc param*(t: Type, i: int): Type {.inline.} =
  assert t.kind == tyFunction
  t.arguments.unbox.nth(i)

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
  of tmEqual, tmNone, tmSimilar, tmAlmostEqual, tmUnknown: tm
  of tmTrue: tmFalse
  of tmFalse: tmTrue
  of tmFiniteTrue, tmGeneric: tmFiniteFalse
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
      match(+matcher.elementType.unbox, t.elementType.unbox)
    of tyTuple:
      # XXX (2) unorderedFields
      # (name: string, age: int) is named tuple vs (name: string anywhere, age: int anywhere) is typeclass but also type of function call arguments
      # second is strict subtype, like (name: string: 1, age: int: 2) vs (name: string: {1, 2}, age: int: {1, 2})
      if matcher.elements.len != t.elements.len and matcher.varargs.isNone and t.varargs.isNone:
        return tmNone
      var max = t.elements.len
      if matcher.elements.len > t.elements.len and (max = matcher.elements.len; t.varargs.isNone):
        return tmNone
      var res = tmAlmostEqual
      for i in 0 ..< max:
        let m = match(+matcher.nth(i), t.nth(i))
        if m < res: res = m
        if res <= tmNone: return res
      if not matcher.varargs.isNone and not t.varargs.isNone:
        let vm = match(+matcher.varargs.unbox, t.varargs.unbox)
        if vm < res: res = vm
      res
    of tyFunction:
      min(
        match(-matcher.returnType.unbox, t.returnType.unbox),
        match(+matcher.arguments.unbox, t.arguments.unbox))
    of tyTable:
      min(
        match(+matcher.keyType.unbox, t.keyType.unbox),
        match(+matcher.valueType.unbox, t.valueType.unbox))
    of tyType:
      match(+matcher.typeValue.unbox, t.typeValue.unbox)
    of allTypeKinds - concreteTypeKinds: tmUnknown # unreachable
  of tyAny: tmTrue
  of tyNone: tmUnknown
  of tyUnion:
    var max = tmFiniteFalse
    for a in matcher.operands:
      let m = match(+a, t)
      if m > max: max = m
      if max >= tmFiniteTrue:
        max = tmFiniteTrue
        break
    max
  of tyIntersection:
    var min = tmFiniteTrue
    for a in matcher.operands:
      let m = match(+a, t)
      if m < min: min = m
      if min <= tmFiniteFalse:
        min = tmFiniteFalse
        break
    min
  of tyNot:
    boolMatch not match(matcher.notType.unbox, t).matches
  of tyBaseType:
    boolMatch t.kind == matcher.baseKind
  of tyCustomMatcher:
    if matcher.typeMatcher.isNil:
      tmNone
    else:
      matcher.typeMatcher(t)
  of tyWithProperty:
    min(
      if not t.properties.hasKey(matcher.withProperty): tmFiniteFalse else: tmAlmostEqual,
      match(+matcher.typeWithProperty.unbox, t))
  of tyParameter:
    min(
      tmGeneric,
      match(matcher.parameter.bound, t))
  #of tyGeneric:
  #  min(
  #    tmGeneric,
  #    match(matcher.genericPattern[], t))
  result = min(result, tmAlmostEqual)
  if result.matches:
    for p, args in matcher.properties:
      if not p.typeMatcher.isNil:
        result = min(result, p.typeMatcher(t, args))
        if result <= tmNone: return result

proc compare*(m1, m2: TypeMatch): int {.inline.} =
  ord(m1) - ord(m2)

proc compare*(t1, t2: Type): int =
  ## t1 < t2 mirrors being a subtype
  # XXX (6) maybe add more logic to this? the current system might be too ambiguous
  let
    m1 = t1.match(t2)
    m2 = t2.match(t1)
  assert not (m1 == tmEqual and m1 != m2), "equal match must be commutative"
  compare(m1, m2)

proc `<`*(a, b: Type): bool {.inline.} = compare(a, b) < 0
proc `<=`*(a, b: Type): bool {.inline.} = compare(a, b) <= 0
proc `>`*(a, b: Type): bool {.inline.} = compare(a, b) > 0
proc `>=`*(a, b: Type): bool {.inline.} = compare(a, b) >= 0

proc commonSubType*(a, b: Type, doUnion = true, variance = Covariant): Type =
  var m1, m2: TypeMatch
  if variance == Covariant:
    m1 = a.match(b)
    m2 = b.match(a)
  else:
    m1 = (a * variance).match(b)
    m2 = (b * variance).match(a)
  let cmp = compare(m1, m2)
  if cmp > 0:
    b
  elif cmp < 0:
    a
  elif m1 in {tmEqual, tmAlmostEqual}:
    a
  elif doUnion: # union here meaning either
    union(a, b)
  else:
    Ty(None)

proc commonSuperType*(a, b: Type, doUnion = true, variance = Covariant): Type =
  var m1, m2: TypeMatch
  if variance == Covariant:
    m1 = a.match(b)
    m2 = b.match(a)
  else:
    m1 = (a * variance).match(b)
    m2 = (b * variance).match(a)
  let cmp = compare(m1, m2)
  if cmp > 0:
    a
  elif cmp < 0:
    b
  elif m1 in {tmEqual, tmAlmostEqual}:
    a
  elif doUnion:
    union(a, b)
  else:
    Ty(None)

import arrays

proc checkType*(value: Value, t: Type): bool
  ## this sucks

proc checkType*(value: FullValueObj, t: Type): bool =
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
  template eachAreTable(iter; kty, vty: Type): untyped =
    let kt = kty; let vt = vty; var yes = true
    for key, value in iter:
      if not checkType(key, kt) or not checkType(value, vt):
        yes = false; break
    yes
  result = case t.kind
  of tyNoneValue: value.kind == vkNone
  of tyInt32: value.kind == vkInt32
  of tyUint32: value.kind == vkUint32
  of tyFloat32: value.kind == vkFloat32
  of tyBool: value.kind == vkBool
  of tyInt64: value.kind == vkInt64
  of tyUint64: value.kind == vkUint64
  of tyFloat64: value.kind == vkFloat64
  of tyFunction:
    # XXX (4) no information about signature
    value.kind in {vkFunction, vkNativeFunction}
  of tyTuple:
    value.kind == vkArray and value.tupleValue.unref.eachAre(t.elements)
  of tyReference:
    value.kind == vkReference and value.referenceValue.unref.checkType(t.elementType.unbox)
  of tyList:
    value.kind == vkList and value.listValue.unref.eachAre(t.elementType.unbox)
  of tyString: value.kind == vkString
  of tySet:
    value.kind == vkSet and value.setValue.eachAre(t.elementType.unbox)
  of tyTable:
    value.kind == vkTable and value.tableValue.eachAreTable(t.keyType.unbox, t.valueType.unbox)
  of tyExpression: value.kind == vkExpression
  of tyStatement: value.kind == vkStatement
  of tyScope: value.kind == vkScope
  of tyType: value.kind == vkType and t.typeValue.unbox.match(value.typeValue).matches
  of tyAny: true
  of tyNone: false
  of tyUnion:
    var res = false
    for ty in t.operands:
      if value.checkType(ty):
        res = true
        break
    res
  of tyIntersection:
    var res = true
    for ty in t.operands:
      if not value.checkType(ty):
        res = false
        break
    res
  of tyNot: not value.checkType(t.notType.unbox)
  of tyBaseType:
    # please remove this type eventually
    type Res = enum unknown, knownTrue, knownFalse
    var res = unknown
    var vkinds: set[ValueKind]
    template vkind(vk: ValueKind) = vkinds = {vk}
    case t.baseKind
    of tyNoneValue: vkind vkNone
    of tyInt32: vkind vkInt32
    of tyUint32: vkind vkUint32
    of tyFloat32: vkind vkFloat32
    of tyBool: vkind vkBool
    of tyInt64: vkind vkInt64
    of tyUint64: vkind vkUint64
    of tyFloat64: vkind vkFloat64
    of tyReference: vkind vkReference
    of tyString: vkind vkString
    of tyExpression: vkind vkExpression
    of tyStatement: vkind vkStatement
    of tyScope: vkind vkScope
    of tyFunction: vkinds = {vkNativeFunction, vkFunction}
    of tyTuple: vkind vkArray
    of tyList: vkind vkList
    of tySet: vkind vkSet
    of tyTable: vkind vkTable
    of tyType: vkind vkType
    of tyAny: res = knownTrue
    of tyNone, tyUnion, tyIntersection, tyNot, tyBaseType, tyWithProperty,
      tyCustomMatcher, tyParameter: res = knownFalse
    case res
    of knownTrue: true
    of knownFalse: false
    of unknown: value.kind in vkinds
  of tyWithProperty:
    value.checkType(t.typeWithProperty.unbox) and value.getType.properties.hasKey(t.withProperty)
  of tyCustomMatcher: not t.valueMatcher.isNil and t.valueMatcher(value.toSmallValue)
  of tyParameter: value.checkType(t.parameter.bound.boundType)
  if result:
    for p, args in t.properties:
      if not p.valueMatcher.isNil:
        result = result and p.valueMatcher(value.toSmallValue, args)
        if not result: return result

proc checkType*(value: Value, t: Type): bool =
  let fvo = value.toFullValueObj
  checkType(fvo, t)

type
  GenericMatchError* = object of TypeError
    parameter*: TypeParameter
    presumed*: Type
    conflicting*: Type

proc newTypeParameter*(name: string, bound: TypeBound = +Ty(Any)): TypeParameter =
  TypeParameter(id: newTypeParameterId(), name: name, bound: bound)

proc matchParameters*(pattern, t: Type, table: var ParameterInstantiation, variance = Covariant) =
  template match(a, b: Type) = matchParameters(a, b, table)
  template match(a, b: Box[Type]) = matchParameters(a.unbox, b.unbox, table)
  case pattern.kind
  of tyParameter:
    let param = pattern.parameter
    if param in table:
      let oldType = table[param]
      let newType = commonSuperType(oldType, t, doUnion = false, variance = variance)
      if newType.isNone:
        raise (ref GenericMatchError)(
          msg: "param " & $param & " had type " & $newType & " but got " & $t,
          parameter: param,
          presumed: oldType,
          conflicting: t)
      table[param] = newType
    else:
      table[param] = t
  of tyNoneValue,
    tyInt32, tyUint32, tyFloat32, tyBool,
    tyInt64, tyUint64, tyFloat64,
    tyString, tyExpression, tyStatement, tyScope,
    tyAny, tyNone:
    discard # atoms
  of tyFunction:
    if t.kind == tyFunction:
      match(pattern.arguments, t.arguments)
      matchParameters(pattern.returnType.unbox, t.returnType.unbox, table, variance = Contravariant)
  of tyTuple:
    if t.kind == tyTuple:
      # XXX (2) unorderedFields
      let
        pl = pattern.elements.len
        tl = t.elements.len
      if pl == tl or not (t.varargs.isNone and pattern.varargs.isNone):
        for i in 0 ..< min(pl, tl):
          match(pattern.nth(i), t.elements[i])
        if pl > tl and not t.varargs.isNone:
          for i in tl ..< pl:
            match(pattern.elements[i], t.varargs.unbox)
          if not pattern.varargs.isNone:
            match(pattern.varargs, t.varargs)
  of tyReference, tyList, tySet:
    if t.kind == pattern.kind:
      match(pattern.elementType, t.elementType)
  of tyTable:
    if t.kind == pattern.kind:
      match(pattern.keyType, t.keyType)
      match(pattern.valueType, t.valueType)
  of tyType:
    if t.kind == pattern.kind:
      match(pattern.typeValue, t.typeValue)
    else:
      # wonky
      match(pattern.typeValue.unbox, t)
  of tyUnion, tyIntersection, tyNot:
    discard # should not be able to match anything
  of tyWithProperty:
    if t.kind == pattern.kind:
      match(pattern.typeWithProperty, t.typeWithProperty)
    else:
      match(pattern.typeWithProperty.unbox, t)
  of tyBaseType, tyCustomMatcher:
    discard # no type to traverse
  for a, v in pattern.properties:
    if unlikely(not a.genericMatcher.isNil):
      a.genericMatcher(pattern, v, t, table, variance)

proc fillParameters*(pattern: var Type, table: ParameterInstantiation) =
  template fill(a: var Type) = fillParameters(a, table)
  template fill(a: var Box[Type]) =
    if not a.isNil:
      var newType: Type = a.unbox
      fillParameters(newType, table)
      a = newType.box
  case pattern.kind
  of tyParameter:
    pattern = table[pattern.parameter]
  of tyNoneValue,
    tyInt32, tyUint32, tyFloat32, tyBool,
    tyInt64, tyUint64, tyFloat64,
    tyString, tyExpression, tyStatement, tyScope,
    tyAny, tyNone, tyBaseType, tyCustomMatcher:
    discard
  of tyFunction:
    fill(pattern.arguments)
    fill(pattern.returnType)
  of tyTuple:
    # XXX (2) unorderedFields
    for e in pattern.elements.mitems:
      fill(e)
    fill(pattern.varargs)
  of tyReference, tyList, tySet:
    fill(pattern.elementType)
  of tyTable:
    fill(pattern.keyType)
    fill(pattern.valueType)
  of tyType:
    fill(pattern.typeValue)
  of tyUnion, tyIntersection:
    for o in pattern.operands.mitems:
      fill(o)
  of tyNot:
    fill(pattern.notType)
  of tyWithProperty:
    fill(pattern.typeWithProperty)
  for a, v in pattern.properties.mpairs:
    if unlikely(not a.genericFiller.isNil):
      a.genericFiller(pattern, v, table)
