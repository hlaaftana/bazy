import
  std/tables,
  ./[primitives, typebasics, typematch] 

type
  GenericMatchError* = object of TypeMatchError
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
      if newType.isNoType:
        raise (ref GenericMatchError)(
          msg: "param " & $param & " had type " & $newType & " but got " & $t,
          parameter: param,
          presumed: oldType,
          conflicting: t)
      table[param] = newType
    else:
      table[param] = t
  of tyNoType, tyAny, tyNone:
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
      if pl == tl or not (t.varargs.isNoType and pattern.varargs.isNoType):
        for i in 0 ..< min(pl, tl):
          match(pattern.nth(i), t.elements[i])
        if pl > tl and not t.varargs.isNoType:
          for i in tl ..< pl:
            match(pattern.elements[i], t.varargs.unbox)
          if not pattern.varargs.isNoType:
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
  of tyNoType, tyAny, tyNone, tyBase:
    discard
  of tyCompound:
    # XXX (3) check argument bounds
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
