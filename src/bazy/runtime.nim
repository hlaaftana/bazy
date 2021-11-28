import tables

# would super prefer not to use a different runtime
# the transition from prototyped to real application code should be as smooth as possible

# type system should exist for both static and runtime overloads
# meaning it can also be used for general pattern matching
# runtime-only types should only be subtypes of static-only types

type
  ValueKind* = enum
    vkNone # some kind of null value
    vkInteger, vkUnsigned, vkFloat # not sure of size
    vkFunction # argument should be tuple?
    vkTuple # like java array but typed like TS, not necessarily hetero or homogenously typed
    vkReference # reference to value
    vkString, vkSeq # references to string and seq of value (string is general byte seq)
    vkComposite # like tuple, but fields are tied to names and unordered
    vkNominalTyped # value with an attached nominal type, unfortunately this is pointer to save memory

  Value* {.acyclic.} = object
    # entire thing can be pointer tagged, but would need GC hooks
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: int64
    of vkUnsigned:
      unsignedValue*: uint64
    of vkFloat:
      floatValue*: float
    of vkFunction:
      functionValue*: proc (args: sink seq[Value]): Value
    of vkTuple:
      # supposed to be just length and pointer, might do 16 bits length 48 bits pointer
      tupleValue*: ref seq[Value]
    of vkReference:
      referenceValue*: ref Value
    of vkString:
      stringValue*: ref string
    of vkSeq:
      seqValue*: ref seq[Value]
    of vkComposite:
      # supposed to be represented more efficiently
      compositeValue*: ref Table[string, Value]
    of vkNominalTyped:
      nominalValue*: ref NominalTypedValue

  TypeKind* = enum
    # concrete
    tyNone,
    tyInteger, tyUnsigned, tyFloat,
    tyFunction, tyTuple,
    tyReference,
    tyString, tySeq,
    tyComposite,
    tyRuntimeNominal,
    # static information
    tyStaticNominal,
    # typeclass
    tyAny, tyUnion, tyIntersection, tyNot,
    tyBaseType, tyCustomMatcher

  NominalTypeKind* = enum
    ntDistinct, ntEnum, ntObject

  NominalType* = ref object
    name*: string
    case kind*: NominalTypeKind
    of ntDistinct, ntEnum, ntObject: discard

  NominalTypedValue* = object
    nominalType*: NominalType
    value*: Value
  
  Type* = ref object
    case kind*: TypeKind
    of tyNone, tyInteger, tyUnsigned, tyFloat,
      tyString, tyAny:
      discard
    of tyFunction:
      arguments*: seq[Type]
      returnType*: Type
    of tyTuple:
      elements*: seq[Type]
    of tySeq, tyReference:
      elementType*: Type
    of tyComposite:
      fields*: Table[string, type]
    of tyRuntimeNominal, tyStaticNominal:
      nominal*: NominalType
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: Type
    of tyBaseType:
      baseKind*: TypeKind
    of tyCustomMatcher:
      check*: proc (t: Type): bool
      compare*: proc (t, other: Type): bool
