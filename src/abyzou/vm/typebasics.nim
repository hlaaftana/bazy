import std/tables, ./[primitives, ids]

template NoType*: untyped = Type(kind: tyNoType)
template AnyTy*: untyped = Type(kind: tyAny)
template NoneTy*: untyped = Type(kind: tyNone)

proc `+`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Covariant)
proc `-`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Contravariant)
proc `~`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Invariant)
proc `*`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: Ultravariant)
proc `*`*(t: Type, variance: Variance): TypeBound {.inline.} = TypeBound(boundType: t, variance: variance)

proc newTypeParameter*(name: string, bound: TypeBound = +AnyTy): TypeParameter =
  TypeParameter(id: newTypeParameterId(), name: name, bound: bound)

when defined(gcDestructors):
  template defineTypeBase*(name, value): untyped {.dirty.} =
    bind newTypeBaseId
    let `name`* = block: # !global
      var `name` {.inject.}: TypeBase
      `name` = value
      `name`.id = newTypeBaseId()
      `name`
else:
  template defineTypeBase*(name, value): untyped =
    bind newTypeBaseId
    proc getTypeBase: TypeBase {.gensym.} =
      var `name` {.global, inject.}: TypeBase
      if `name`.isNil:
        `name` = value
        `name`.id = newTypeBaseId()
      result = `name`
    template `name`*: TypeBase {.inject.} = getTypeBase()

proc toTypeParams(args: varargs[TypeBound]): seq[TypeParameter] =
  result.newSeq(args.len)
  for i in 0 ..< args.len:
    result[i] = newTypeParameter("", args[i])

template nativeType(n: untyped, nt: NativeType, args: varargs[TypeBound]) =
  defineTypeBase n, TypeBase(name: astToStr(n),
    nativeType: nt,
    arguments: toTypeParams(args))

template nativeType(n: untyped, args: varargs[TypeBound]) =
  nativeType(`n Ty`, `nty n`, args)

template nativeAtomicType(n: untyped) =
  nativeType(`n TyBase`, `nty n`)
  template `n Ty`*: untyped = Type(kind: tyCompound, base: `n TyBase`)

nativeType TupleTy, ntyTuple, [] # not used for tuple type construction

nativeAtomicType NoneValue
nativeAtomicType Int32
nativeAtomicType Uint32
nativeAtomicType Float32
nativeAtomicType Bool
nativeAtomicType Int64
nativeAtomicType Uint64
nativeAtomicType Float64
nativeAtomicType String
nativeAtomicType Expression
nativeAtomicType Statement
nativeAtomicType Scope
nativeType Reference, [+AnyTy]
nativeType List, [+AnyTy]
nativeType Set, [+AnyTy]
nativeType Table, [+AnyTy, +AnyTy]
nativeType Function, [+Type(kind: tyBase, typeBase: TupleTy), -NoneTy]
  # XXX (2) account for Fields and Defaults property of the `arguments` tuple type
  # only considered at callsite like nim, no semantic value
  # meaning this is specific to function type relation
nativeType Type, [+AnyTy]

nativeType ContravariantTy, ntyContravariant, [+AnyTy]

proc compound*(tag: TypeBase, args: varargs[Type]): Type {.inline.} =
  Type(kind: tyCompound, base: tag, baseArguments: @args)

template `!`*(tag: TypeBase): Type = compound(tag)
template `[]`*(tag: TypeBase, args: varargs[Type]): Type = compound(tag, args)

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
  tyNoType: 0,
  tyCompound: -1,
  tyTuple: -1,
  tyAny: 0,
  tyNone: 0,
  tyUnion: -1,
  tyIntersection: -1,
  tyNot: 1,
  tyWithProperty: -1,
  tyBase: 0,
  tySomeValue: 1,
  tyParameter: -1,
  tyValue: -1
]

proc len*(t: Type): int =
  result = definiteTypeLengths[t.kind]
  if result < 0:
    case t.kind
    of tyTuple:
      if t.varargs.isNoType:
        result = t.elements.len + t.unorderedFields.len
    of tyCompound:
      result = t.baseArguments.len
    of tyUnion, tyIntersection:
      result = t.operands.len
    else: discard

proc hasNth*(t: Type, i: int): bool {.inline.} =
  i < t.len or (t.kind == tyTuple and not t.varargs.isNoType)

proc nth*(t: Type, i: int): Type =
  case t.kind
  of tyNoType, tyAny, tyNone:
    discard # inapplicable
  of tyTuple:
    if i < t.elements.len or t.varargs.isNoType:
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
  of tyParameter, tyValue:
    discard # what

proc param*(t: Type, i: int): Type {.inline.} =
  assert t.kind == tyCompound and t.base.nativeType == ntyFunction
  t.baseArguments[0].nth(i)
