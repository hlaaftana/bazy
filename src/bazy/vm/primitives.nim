import std/[tables, sets, hashes], "."/[arrays, pointertag]

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
    vkShortTuple
      ## same as tuple but caps out at 255 size so it can be a single pointer
      ## that points to a length and then elements
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
      ## Nim function that takes values as argument
    vkFunction
      ## function
    vkEffect
      ## embedded effect value for exceptions/return/yield/whatever
    # sets/tables/bigints can be added

  UniqueReference* = distinct ref Value

  RuntimePropertyObj* = object
    properties*: HashSet[Value]
    value*: Value

  ValueObj* = object
    # entire thing can be pointer tagged, but would need GC hooks
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: BiggestInt
    of vkUnsigned:
      unsignedValue*: BiggestUInt
    of vkFloat:
      floatValue*: float
    of vkSeq:
      seqValue*: ref seq[Value]
    of vkString:
      stringValue*: ref string
    of vkTuple:
      tupleValue*: ref Array[Value]
    of vkShortTuple:
      shortTupleValue*: ShortArray[Value]
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
    of vkEffect:
      effectValue*: ref Value
  
  PointerTaggedValue* = distinct (
    when pointerTaggable:
      TaggedPointer
    else:
      Value
  )
  
  Value* = ValueObj

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

  Stack* = ref object
    imports*: Array[Stack]
    stack*: Array[Value]

  Function* = ref object
    imports*: Array[Stack]
    stackSize*: int
    instruction*: Instruction

  InstructionKind* = enum
    Constant
    FunctionCall
    Sequence
    # stack
    VariableGet
    VariableSet
    FromImportedStack
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
    of FromImportedStack:
      importedStackIndex*: int
      importedStackInstruction*: Instruction
    of If:
      ifCondition*, ifTrue*, ifFalse*: Instruction
    of While:
      whileCondition*, whileTrue*: Instruction
    of DoUntil:
      doUntilCondition*, doUntilTrue*: Instruction
    of EmitEffect:
      effect*: Instruction
    of HandleEffect:
      effectHandler*, effectEmitter*: Instruction

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

static:
  doAssert sizeof(Value) <= 2 * sizeof(int)

proc fromValueObj*(v: ValueObj): PointerTaggedValue =
  when pointerTaggable:
    result = PointerTaggedValue:
      case v.kind
      of vkNone: 0'u64
      of vkInteger: (v.kind.uint64 shl 48) or int32(v.integerValue).uint64
      of vkUnsigned: (v.kind.uint64 shl 48) or int32(v.integerValue).uint64
      of vkFloat: (v.kind.uint64 shl 48) or int32(v.integerValue).uint64
      of vkSeq: cast[pointer](v.seqValue).tagPointer(v.kind.uint16)
      of vkString: cast[pointer](v.stringValue).tagPointer(v.kind.uint16)
      of vkTuple: cast[pointer](v.tupleValue).tagPointer(v.kind.uint16)
      of vkShortTuple: cast[pointer](v.shortTupleValue).tagPointer(v.kind.uint16)
      of vkReference: cast[pointer](v.referenceValue).tagPointer(v.kind.uint16)
      of vkUniqueReference: cast[pointer](v.uniqueReferenceValue).tagPointer(v.kind.uint16)
      of vkComposite: cast[pointer](v.compositeValue).tagPointer(v.kind.uint16)
      of vkPropertyReference: cast[pointer](v.propertyRefValue).tagPointer(v.kind.uint16)
      of vkType: cast[pointer](v.typeValue).tagPointer(v.kind.uint16)
      of vkNativeFunction: cast[pointer](v.nativeFunctionValue).tagPointer(v.kind.uint16)
      of vkFunction: cast[pointer](v.functionValue).tagPointer(v.kind.uint16)
      of vkEffect: cast[pointer](v.effectValue).tagPointer(v.kind.uint16)
  else:
    v.PointerTaggedValue

proc toValueObj*(p: PointerTaggedValue): ValueObj =
  when pointerTaggable:
    let val = p.uint64
    result.kind = ValueKind(val shr 48)
    case result.kind
    of vkNone: discard
    of vkInteger: result.integerValue = (val and high(uint32).uint64).int32.int
    of vkUnsigned: result.unsignedValue = (val and high(uint32).uint64).uint
    of vkFloat: result.floatValue = (val and high(uint32).uint64).float32.float
    of vkSeq: result.seqValue = cast[typeof(result.seqValue)](untagPointer(val))
    of vkString: result.stringValue = cast[typeof(result.stringValue)](untagPointer(val))
    of vkTuple: result.tupleValue = cast[typeof(result.tupleValue)](untagPointer(val))
    of vkShortTuple: result.shortTupleValue = cast[typeof(result.shortTupleValue)](untagPointer(val))
    of vkReference: result.referenceValue = cast[typeof(result.referenceValue)](untagPointer(val))
    of vkUniqueReference: result.uniqueReferenceValue = cast[typeof(result.uniqueReferenceValue)](untagPointer(val))
    of vkComposite: result.compositeValue = cast[typeof(result.compositeValue)](untagPointer(val))
    of vkPropertyReference: result.propertyRefValue = cast[typeof(result.propertyRefValue)](untagPointer(val))
    of vkType: result.typeValue = cast[typeof(result.typeValue)](untagPointer(val))
    of vkNativeFunction: result.nativeFunctionValue = cast[typeof(result.nativeFunctionValue)](untagPointer(val))
    of vkFunction: result.functionValue = cast[typeof(result.functionValue)](untagPointer(val))
    of vkEffect: result.effectValue = cast[typeof(result.effectValue)](untagPointer(val))
  else:
    p.ValueObj
