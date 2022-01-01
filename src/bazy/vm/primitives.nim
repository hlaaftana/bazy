import tables, sets, hashes

# type system should exist for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

{.push acyclic.}

type
  ValueKind* = enum
    vkNone
      ## some kind of null value
    vkInteger, vkUnsigned, vkFloat
      ## size is implementation detail
      ## same as pointer size for now but may be 32 or 48 bits
      ## if values use pointer packing
    vkSeq
      ## both seq and string are references to save memory
    vkString
      ## general byte sequence type
    vkTuple
      ## like java array but typed like TS, not necessarily hetero or homogenously typed
      ## caps out at 255 size so it can be a single pointer that points to a length
      ## and then elements
    vkReference
      ## reference (pointer) to value
    vkUniqueReference
      ## reference to value identified by its address (singleton)
    vkComposite
      ## like tuple, but fields are tied to names and unordered
      ## (really the names define the order and the names can at least be shortstrings)
    vkPropertyReference
      ## references with some type properties attached at runtime
      ## which are also runtime values
    vkType
      ## type value
    vkNativeFunction
    vkFunction

  UniqueReference* = distinct ref Value

  Tuple* = seq[Value]
    # supposed to be just length and pointer like openarray
    # probably will be a pointer with length at 0th byte

  RuntimePropertyObj* = object
    properties*: HashSet[Value]
    value*: Value

  Value* = object
    # entire thing can be pointer tagged, but would need GC hooks
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: int64
    of vkUnsigned:
      unsignedValue*: uint64
    of vkFloat:
      floatValue*: float
    of vkSeq:
      seqValue*: ref seq[Value]
    of vkString:
      stringValue*: ref string
    of vkTuple:
      tupleValue*: ref Tuple
    of vkReference:
      referenceValue*: ref Value
    of vkUniqueReference:
      uniqueReferenceValue*: UniqueReference
    of vkComposite:
      # supposed to be represented more efficiently
      # short string instead of string
      # compare composite keys by hashing keys and caching it
      compositeValue*: ref Table[string, Value]
    of vkPropertyReference:
      propertyRefValue*: ref RuntimePropertyObj
    of vkType:
      typeValue*: ref Type
    of vkFunction:
      functionValue*: Function
    of vkNativeFunction:
      nativeFunctionValue*: proc (args: openarray[Value]): Value {.nimcall.}
      # replace with single value argument?

  Stack* = Tuple

  Function* = ref object
    imports*: seq[Stack]
    stackSize*: int
    instruction*: Instruction

  InstructionKind* = enum
    Constant
    FunctionCall
    Sequence
    # stack
    VariableGet
    VariableSet
    # goto
    If
    While
    DoUntil
    # effect, can emulate goto
    EmitEffect
    HandleEffect

  Instruction* = ref object
    case kind*: InstructionKind
    of Constant:
      constantValue*: Value
    of FunctionCall:
      function*: Instruction # evaluates to Function or native function
      arguments*: seq[Instruction]
    of Sequence:
      sequence*: seq[Instruction]
    of VariableGet:
      variableGetIndex*: int
    of VariableSet:
      variableSetIndex*: int
      variableSetValue*: Instruction
    of If:
      ifCondition, ifTrue, ifFalse: Instruction
    of While:
      whileCondition, whileTrue: Instruction
    of DoUntil:
      doUntilCondition, doUntilTrue: Instruction
    of EmitEffect:
      effect*: Instruction
    of HandleEffect:
      effectHandler*: proc (effect: Value): bool
      effectEmitter*: Instruction

  TypeKind* = enum
    # concrete
    tyNoneValue,
    tyInteger, tyUnsigned, tyFloat,
    tyFunction, tyTuple,
    tyReference,
    tySeq,
    tyString,
    tyComposite,
    tyType,
    # typeclass
    tyAny, tyNone, tyUnion, tyIntersection, tyNot,
    tyBaseType, tyContainingProperty,
    # matcher
    tyCustomMatcher
  
  Type* = object
    properties*: HashSet[Value]
    case kind*: TypeKind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat,
      tyString, tyAny, tyNone:
      discard
    of tyFunction:
      arguments*: seq[Type]
      returnType*: ref Type
    of tyTuple:
      elements*: seq[Type]
    of tyReference, tySeq:
      elementType*: ref Type
    of tyComposite:
      fields*: Table[string, Type]
    of tyType:
      typeValue*: ref Type
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: ref Type
    of tyBaseType:
      baseKind*: TypeKind
    of tyContainingProperty:
      containingProperty*: Value
    of tyCustomMatcher:
      typeMatcher*: proc (t: Type): bool
      valueMatcher*: proc (v: Value): bool
      #compare*: proc (otherMatcher, t: Type): bool
        # if closer to `t` than `otherMatcher`, return true

{.pop.} # acyclic

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyNoneValue..tyType}
  typeclassTypeKinds* = {tyAny..tyContainingProperty}
  matcherTypeKinds* = typeclassTypeKinds + {tyCustomMatcher}

import util/objects

template mix(x) =
  result = result !& hash(x)

proc hash(r: ref): Hash =
  mix hash(r[])
  result = !$ result

proc hash*(p: UniqueReference): Hash =
  mix cast[pointer]((ref Value)(p))
  result = !$ result

proc hash*(v: Value | Type): Hash =
  for f in fields(v):
    mix f
  result = !$ result

proc `==`*(a, b: Value): bool
proc `==`*(a, b: Type): bool
  
defineRefEquality Value
defineRefEquality Type

template `==`*(p1, p2: UniqueReference): bool =
  system.`==`((ref Value)(p1), (ref Value)(p2))

defineEquality Value
defineEquality Type
