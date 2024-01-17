import
  std/[tables, sets],
  ./[primitives, typebasics, typematch, arrays, valueconstr, guesstype]

proc checkType*(value: Value, t: Type): bool

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
      value.kind in {vkFunction, vkNativeFunction, vkLinearFunction}
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
  of tyNone, tyNoType: false
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
