import
  std/[tables, sets],
  ./[primitives, ids, typebasics]

type TypeMatchError* = object of CatchableError

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyTuple}
  typeclassTypeKinds* = {tyAny..tySomeValue}
  allNativeTypes* = {low(NativeType)..high(NativeType)}
  concreteNativeTypes* = {ntyNoneValue..ntyType}
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
