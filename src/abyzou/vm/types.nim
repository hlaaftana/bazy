import "."/[primitives, values, ids], std/[tables, sets]

proc property*(tag: TypeBase, args: varargs[Type]): Type {.inline.} =
  compound(tag, args)

proc property*(prop: Type): Type {.inline.} =
  assert prop.kind == tyCompound
  prop

proc properties*(ps: varargs[Type, property]): Table[TypeBase, Type] =
  result = initTable[TypeBase, Type](ps.len)
  for p in ps:
    result[p.base] = p

proc withProperties*(ty: sink Type, ps: varargs[Type, property]): Type {.inline.} =
  ty.properties = properties(ps)
  ty

proc hasProperty*(t: Type, tag: TypeBase): bool =
  t.properties.hasKey(tag)

proc property*(t: Type, tag: TypeBase): Type =
  t.properties[tag]

type TypeError* = object of CatchableError

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyTuple}
  typeclassTypeKinds* = {tyAny..tySomeValue}
  allNativeTypes* = {low(NativeType)..high(NativeType)}
  concreteNativeTypes* = {ntyNoneValue..ntyType}
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc tupleType*(s: varargs[Type]): Type =
  Type(kind: tyTuple, elements: @s)

proc funcType*(returnType: Type, arguments: varargs[Type]): Type {.inline.} =
  FunctionTy[tupleType(arguments), returnType]

proc tupleTypeWithVarargs*(s: varargs[Type], varargs: Type): Type =
  Type(kind: tyTuple, elements: @s, varargs: varargs.box)

proc funcTypeWithVarargs*(returnType: Type, arguments: varargs[Type], varargs: Type): Type {.inline.} =
  FunctionTy[tupleTypeWithVarargs(arguments, varargs), returnType]

proc union*(s: varargs[Type]): Type =
  Type(kind: tyUnion, operands: @s)

const definiteTypeLengths*: array[TypeKind, int] = [
  tyNone: 0,
  tyCompound: -1,
  tyTuple: -1,
  tyAny: 0,
  tyUnion: -1,
  tyIntersection: -1,
  tyNot: 1,
  tyWithProperty: -1,
  tyBase: 0,
  tySomeValue: 1,
  tyParameter: -1,
  #tyGeneric: -1,
  tyValue: -1
]

proc len*(t: Type): int =
  result = definiteTypeLengths[t.kind]
  if result < 0:
    case t.kind
    of tyTuple:
      if t.varargs.isNone:
        result = t.elements.len + t.unorderedFields.len
    of tyCompound:
      result = t.baseArguments.len
    of tyUnion, tyIntersection:
      result = t.operands.len
    else: discard

proc hasNth*(t: Type, i: int): bool {.inline.} =
  i < t.len or (t.kind == tyTuple and not t.varargs.isNone)

proc nth*(t: Type, i: int): Type =
  case t.kind
  of tyAny, tyNone:
    discard # inapplicable
  of tyTuple:
    if i < t.elements.len or t.varargs.isNone:
      result = t.elements[i]
    else:
      result = t.varargs.unbox
  of tyCompound:
    result = t.baseArguments[i]
  of tyUnion, tyIntersection:
    # this is actually not supposed to happen
    result = t.operands[i]
  of tyNot:
    result = t.notType.unbox
  of tyWithProperty:
    discard # inapplicable
  of tyBase:
    discard # inapplicable
  of tySomeValue:
    result = t.someValueType.unbox
  of tyParameter, tyValue:#, tyGeneric:
    discard # what

proc param*(t: Type, i: int): Type {.inline.} =
  assert t.kind == tyCompound and t.base.nativeType == ntyFunction
  t.baseArguments[0].nth(i)

proc matches*(tm: TypeMatch): bool {.inline.} =
  tm >= lowestMatching

proc boolMatch*(b: bool): TypeMatch {.inline.} =
  if b: tmTrue else: tmFalse

template min*(a, b: TypeMatch): TypeMatch =
  let am = a
  (if am == tmNone: am
  else: system.min(am, b))

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
  of tyCompound:
    case matcher.base.nativeType
    of ntyContravariant:
      match(-matcher.baseArguments[0], t)
    elif unlikely(not matcher.base.typeMatcher.isNil):
      matcher.base.typeMatcher(matcher, t)
    else:
      case t.kind
      of tyCompound:
        let mnt = matcher.base.nativeType
        let tnt = t.base.nativeType
        if mnt != tnt:
          return if {mnt, tnt} <= concreteNativeTypes:
            tmNone
          else:
            tmUnknown
      # XXX handle native types in TypeKind
      else: return tmUnknown
      if matcher.base != t.base: return tmUnknown
      var res = tmAlmostEqual
      for i in 0 ..< matcher.base.arguments.len:
        let v = matcher.base.arguments[i].bound.variance
        let m = match(matcher.baseArguments[i] * v, t.baseArguments[i])
        if m < res: res = m
        if res <= tmNone: return res
      res
  of tyTuple:
    # XXX (2) unorderedFields
    # (name: string, age: int) is named tuple vs (name: string anywhere, age: int anywhere) is typeclass but also type of function call arguments
    # second is strict subtype, like (name: string: 1, age: int: 2) vs (name: string: {1, 2}, age: int: {1, 2})
    if matcher.kind != t.kind:
      return tmUnknown
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
  of tyBase:
    if matcher.typeBase.nativeType == ntyTuple and t.kind == tyTuple:
      return tmTrue
    # in nim a dummy compound type is created from the base and compared
    case t.kind
    of tyBase:
      if matcher.typeBase == t.typeBase: tmAlmostEqual
      else: tmNone
    of tyCompound:
      boolMatch matcher.typeBase == t.typeBase
    else: tmNone
  of tySomeValue:
    case t.kind
    of tySomeValue:
      min(
        tmAlmostEqual,
        match(+matcher.someValueType.unbox, t.someValueType.unbox))
    of tyValue:
      min(
        tmSimilar,
        match(+matcher.someValueType.unbox, t.valueType.unbox))
    else: tmNone
  of tyWithProperty:
    min(
      if not t.properties.hasKey(matcher.withProperty): tmFiniteFalse else: tmAlmostEqual,
      match(+matcher.typeWithProperty.unbox, t))
  of tyParameter:
    min(
      tmGeneric,
      match(matcher.parameter.bound, t))
  of tyValue:
    case t.kind
    of tyValue:
      let tm = match(matcher.valueType.unbox, t.valueType.unbox)
      if not tm.matches or matcher.value != t.value:
        tmNone
      else: tm
    of tySomeValue: tmUnknown
    else: tmNone
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
    Type(kind: tyNone)

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
    Type(kind: tyNone)

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
  of tyCompound, tyBase:
    let b = if t.kind == tyCompound: t.base else: t.typeBase
    case b.nativeType
    of ntyNoneValue: value.kind == vkNone
    of ntyInt32: value.kind == vkInt32
    of ntyUint32: value.kind == vkUint32
    of ntyFloat32: value.kind == vkFloat32
    of ntyBool: value.kind == vkBool
    of ntyInt64: value.kind == vkInt64
    of ntyUint64: value.kind == vkUint64
    of ntyFloat64: value.kind == vkFloat64
    of ntyFunction:
      # XXX (4) no information about signature
      value.kind in {vkFunction, vkNativeFunction}
    of ntyTuple:
      value.kind == vkArray and (t.kind == tyBase or value.tupleValue.unref.eachAre(t.baseArguments))
    of ntyReference:
      value.kind == vkReference and (t.kind == tyBase or value.referenceValue.unref.checkType(t.baseArguments[0]))
    of ntyList:
      value.kind == vkList and (t.kind == tyBase or value.listValue.unref.eachAre(t.baseArguments[0]))
    of ntyString: value.kind == vkString
    of ntySet:
      value.kind == vkSet and (t.kind == tyBase or value.setValue.eachAre(t.baseArguments[0]))
    of ntyTable:
      value.kind == vkTable and (t.kind == tyBase or value.tableValue.eachAreTable(t.baseArguments[0], t.baseArguments[1]))
    of ntyExpression: value.kind == vkExpression
    of ntyStatement: value.kind == vkStatement
    of ntyScope: value.kind == vkScope
    else:
      not b.valueMatcher.isNil and
        b.valueMatcher(value.toSmallValue, t)
  of tyTuple:
    value.kind == vkArray and value.tupleValue.unref.eachAre(t.elements)
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
  of tyWithProperty:
    value.checkType(t.typeWithProperty.unbox) and value.getType.properties.hasKey(t.withProperty)
  of tySomeValue: false
  of tyParameter: value.checkType(t.parameter.bound.boundType)
  of tyValue: value.checkType(t.valueType.unbox) and t.value.toFullValueObj == value
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
  of tyAny, tyNone:
    discard # atoms
  of tyCompound:
    if unlikely(not pattern.base.genericMatcher.isNil):
      pattern.base.genericMatcher(pattern, t, table, variance)
    else:
      if t.kind == pattern.kind:
        for i in 0 ..< min(pattern.baseArguments.len, t.baseArguments.len):
          match(pattern.baseArguments[i], t.baseArguments[i])
  of tyTuple:
    if t.kind == tyTuple:
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
      # XXX (2) is this enough?
      for name, f in pattern.unorderedFields:
        if name in t.elementNames:
          match(f, t.elements[t.elementNames[name]])
        elif name in t.unorderedFields:
          match(f, t.unorderedFields[name])
      for name, f in t.unorderedFields:
        if name in pattern.elementNames:
          match(pattern.elements[pattern.elementNames[name]], f)
        elif name in pattern.unorderedFields:
          match(pattern.unorderedFields[name], f)
  of tyUnion, tyIntersection, tyNot:
    discard # should not be able to match anything
  of tyWithProperty:
    if t.kind == pattern.kind:
      match(pattern.typeWithProperty, t.typeWithProperty)
    else:
      match(pattern.typeWithProperty.unbox, t)
  of tyBase:
    discard # no type to traverse
  of tyValue:
    if t.kind == pattern.kind:
      match(pattern.valueType, t.valueType)
  of tySomeValue:
    if t.kind == pattern.kind:
      match(pattern.someValueType, t.someValueType)
  for a, v in pattern.properties:
    if t.hasProperty(a):
      match(v, t.properties[a])

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
  of tyAny, tyNone, tyBase:
    discard
  of tyCompound:
    if unlikely(not pattern.base.genericFiller.isNil):
      pattern.base.genericFiller(pattern, table)
    else:
      for t in pattern.baseArguments.mitems:
        fill(t)
  of tyTuple:
    for e in pattern.elements.mitems:
      fill(e)
    fill(pattern.varargs)
    for _, e in pattern.unorderedFields.mpairs:
      fill(e)
  of tyUnion, tyIntersection:
    for o in pattern.operands.mitems:
      fill(o)
  of tyNot:
    fill(pattern.notType)
  of tyWithProperty:
    fill(pattern.typeWithProperty)
  of tyValue:
    fill(pattern.valueType)
  of tySomeValue:
    fill(pattern.someValueType)
  for a, v in pattern.properties.mpairs:
    fill(v)
