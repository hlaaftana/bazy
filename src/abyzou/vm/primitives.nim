import
  std/[tables, sets, hashes],
  ../util/box,
  ./[arrays, pointertag, ids],
  ../language/expressions

export box, unbox
template toRef*[T](x: T): ref T =
  var res: ref T
  new(res)
  res[] = x
  res
type Reference*[T] = distinct ref T
proc `==`*[T](a, b: Reference[T]): bool = system.`==`((ref T)(a), (ref T)(b))
proc hash*[T](a: Reference[T]): Hash =
  result = result !& hash(cast[pointer]((ref T)(a)))
  result = !$ result
template unref*[T](x: ref T): untyped = x[]
template unref*[T](x: Box[T]): untyped = x.unbox
template unref*[T](x: Reference[T]): untyped = (ref T)(x)[]
template unref*[T](x: T): untyped = x
template realRef*[T](x: Reference[T]): ref T = (ref T)(x)

# type system exists for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

type
  ValueKind* = enum
    vkNone
      ## singleton null value
    vkInt32, vkUint32, vkFloat32, vkBool
      ## unboxed numbers
    vkEffect
      ## embedded effect value for exceptions/return/yield/whatever
    #vkShortestString
    #  ## word size string
    vkReference
      ## reference value, can be mutable
      ## only value kind with reference semantics
    vkArray
      ## like java array but typed like TS, implementation of tuples
    vkNativeFunction
      ## Nim function that takes values as argument
    # could be pointers or serialized but for now this is more efficient:
    vkExpression
    vkStatement
    vkScope
    vkBoxed
      ## boxed version of unboxed values, used for type info
    vkInt64, vkUint64, vkFloat64
    vkType
      ## type value
    vkString
      ## general byte sequence type
    vkList
      ## both seq and string are references to save memory
    vkSet
    vkTable
    vkFunction
      ## function
    vkLinearFunction
    # bigints can be added
    # native types in general can be BoxedValue[pointer]

const unboxedValueKinds* = {vkNone..pred(vkBoxed)}

type
  BoxedValueObj*[T] = object
    # value first might be useful
    when T isnot Type:
      value*: T
    `type`*: Type
  BoxedValue*[T] = ref BoxedValueObj[T]

  ValueObj* = object
    # entire thing can be pointer tagged, but would need GC hooks
    # maybe interning for some pointer types
    case kind*: ValueKind
    of vkNone: discard
    of vkBool:
      boolValue*: bool
    of vkInt32:
      int32Value*: int32
    of vkUint32:
      uint32Value*: uint32
    of vkFloat32:
      float32Value*: float32
    of vkEffect:
      effectValue*: Box[Value]
    of vkReference:
      # XXX figure out how to optimize this for mutable collections
      referenceValue*: Reference[Value]
    of vkArray:
      # XXX (6) maybe match pointer field location with vkList, vkString
      arrayValue*: ArrayRef[Value]
    of vkBoxed:
      boxedValue*: BoxedValue[Value]
    of vkInt64:
      int64Value*: BoxedValue[int64]
    of vkUint64:
      uint64Value*: BoxedValue[uint64]
    of vkFloat64:
      float64Value*: BoxedValue[float64]
    of vkType:
      typeValue*: BoxedValue[Type]
    of vkString:
      # XXX (6) maybe match pointer field location with vkArray, vkList
      stringValue*: BoxedValue[string]
    of vkList:
      # XXX (6) maybe match pointer field location with vkArray, vkString
      listValue*: BoxedValue[seq[Value]]
    of vkSet:
      setValue*: BoxedValue[HashSet[Value]]
    of vkTable:
      tableValue*: BoxedValue[Table[Value, Value]]
    of vkFunction:
      functionValue*: BoxedValue[TreeWalkFunction]
    of vkLinearFunction:
      linearFunctionValue*: BoxedValue[LinearFunction]
    of vkNativeFunction:
      nativeFunctionValue*: proc (args: openarray[Value]): Value {.nimcall.}
    of vkExpression:
      expressionValue*: Expression
    of vkStatement:
      statementValue*: Statement
    of vkScope:
      scopeValue*: Scope
  
  PointerTaggedValue* = distinct (
    when pointerTaggable:
      TaggedPointer
    else:
      Value
  )
  
  Value* = ValueObj

  TypeKind* = enum
    # maybe add unknown type for values with unknown type at runtime
    tyNoType,
    # concrete
    tyCompound,
    tyTuple, # XXX (1) make into tyComposite, tuple, named tuple, array (i.e. int^20) all at once
    # typeclass
    tyAny, tyAll, ## top and bottom types
    tyUnion, tyIntersection, tyNot,
    tyBase,
    tySomeValue,
    # generic parameter
    tyParameter,
    # value container
    tyValue
    
  MatchLevel* = enum
    # in order of strength
    tmUnknown, tmNone,
    tmFiniteFalse, tmFalse,
    tmUniversalFalse, tmUniversalTrue
    tmTrue, tmFiniteTrue,
    tmSimilar, tmGeneric,
    tmAlmostEqual, tmEqual
  
  TypeMatch* = object
    level*: MatchLevel
    case deep*: bool
    of false: discard
    of true:
      children*: seq[TypeMatch]

  Variance* = enum
    Covariant
    Contravariant
    Invariant
    Ultravariant
  
  ParameterInstantiation* = object
    strict*: bool
    table*: Table[TypeParameter, Type]

  NativeType* = enum
    ntyNone,
    # weird concrete
    ntyTuple,
    # concrete
    ntyNoneValue,
    ntyInt32, ntyUint32, ntyFloat32, ntyBool,
    ntyInt64, ntyUint64, ntyFloat64,
    ntyReference,
    ntyFunction,
    ntyList,
    ntyString,
    ntySet,
    ntyTable,
    ntyExpression, ntyStatement, ntyScope,
    ntyType,
    # typeclass
    ntyTupleConstructor

  TypeBase* = ref object
    # XXX (2) check arguments at generic fill time
    id*: TypeBaseId
    name*: string
    nativeType*: NativeType
    arguments*: seq[TypeParameter]
    typeMatcher*: proc (pattern, t: Type, inst: var ParameterInstantiation): TypeMatch
    valueMatcher*: proc (v: Value, thisType: Type): bool
    paramFiller*: proc (pattern: var Type, table: ParameterInstantiation)

  TypeParameter* = ref object
    id*: TypeParameterId # this needs to be assigned
    name*: string
    bound*: TypeBound
  
  Type* = object
    # XXX (2) 90 bytes
    # XXX (2) figure out which kinds to merge with tyCompound (XXX (1) at least tyTuple)
    properties*: Table[TypeBase, Type]
      # can be a multitable later on
    case kind*: TypeKind
    of tyNoType, tyAny, tyAll: discard
    of tyCompound:
      base*: TypeBase
      baseArguments*: seq[Type]
    of tyTuple:
      elements*: seq[Type]
      varargs*: Box[Type] # for now only trailing
        # XXX (1) either move to property, or allow non-trailing
        # XXX (1) definite length varargs? i.e. array[3, int]
      elementNames*: Table[string, int]
      # XXX (1) also Defaults purely for initialization/conversion?
      # meaning only considered in function type relation
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: Box[Type]
    of tyBase:
      typeBase*: TypeBase
    of tySomeValue:
      someValueType*: Box[Type]
    of tyParameter:
      parameter*: TypeParameter
    of tyValue:
      value*: Value
      valueType*: Box[Type]

  TypeBound* = object
    boundType*: Type
    variance*: Variance

  Stack* = ref object
    stack*: Array[Value]

  TreeWalkFunction* = object
    stack*: Stack
      ## persistent stack
      ## gets shallow copied when function is run
    instruction*: Instruction
  
  LinearFunction* = object
    registerCount*: int
    argPositions*: Array[int] ## last is result
    constants*: Array[Value] # XXX (5) serialize values
    jumpLocations*: Array[int]
    instructions*: seq[byte]

  InstructionKind* = enum
    NoOp
    Constant
    FunctionCall, Dispatch
    Sequence
    # stack
    VariableGet, VariableSet
    ArmStack
    # goto
    If, While, DoUntil
    # effect, can emulate goto
    EmitEffect, HandleEffect
    # collection
    BuildTuple, BuildList, BuildSet, BuildTable
    GetIndex, SetIndex
    # binary
    AddInt, SubInt, MulInt, DivInt
    AddFloat, SubFloat, MulFloat, DivFloat
    CheckType
    # unary
    NegInt, NegFloat
  
  BinaryInstructionKind* = range[AddInt .. CheckType]
  UnaryInstructionKind* = range[NegInt .. NegFloat]

  InstructionObj* = object ## compact version of Statement
    case kind*: InstructionKind
    of NoOp: discard
    of Constant:
      constantValue*: Value
    of FunctionCall:
      function*: Instruction # evaluates to TreeWalkFunction or native function
      arguments*: Array[Instruction]
    of Dispatch:
      dispatchFunctions*: Array[(Array[Type], Instruction)]
      dispatchArguments*: Array[Instruction]
    of Sequence:
      sequence*: Array[Instruction]
    of VariableGet:
      variableGetIndex*: int
    of VariableSet:
      variableSetIndex*: int
      variableSetValue*: Instruction
    of ArmStack:
      armStackFunction*: Instruction
      armStackCaptures*: Array[tuple[index, valueIndex: int]]
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
    of BuildTuple, BuildList, BuildSet:
      elements*: Array[Instruction]
    of BuildTable:
      entries*: Array[tuple[key, value: Instruction]]
    of GetIndex:
      getIndexAddress*: Instruction
      getIndex*: int
    of SetIndex:
      setIndexAddress*: Instruction
      setIndex*: int
      setIndexValue*: Instruction
    of low(UnaryInstructionKind) .. high(UnaryInstructionKind):
      unary*: Instruction
    of low(BinaryInstructionKind) .. high(BinaryInstructionKind):
      binary1*, binary2*: Instruction
  Instruction* = ref InstructionObj

  StatementKind* = enum
    skNone
    skConstant
    skFunctionCall, skDispatch
    skSequence
    # stack
    skVariableGet, skVariableSet
    skArmStack
    # goto
    skIf, skWhile, skDoUntil
    # effect, can emulate goto
    skEmitEffect, skHandleEffect
    # collections
    skTuple, skList, skSet, skTable
    skGetIndex, skSetIndex
    # custom instructions
    skUnaryInstruction, skBinaryInstruction

  StatementObj* = object
    ## typed/compiled expression
    # XXX (7) differentiate between validated and unvalidated,
    # maybe allow things like skFromExpression for metas
    knownType*: Type
    case kind*: StatementKind
    of skNone: discard
    of skConstant:
      constant*: Value
    of skFunctionCall:
      callee*: Statement
      arguments*: seq[Statement]
    of skDispatch:
      # XXX (8) generalize dispatch result
      dispatchees*: seq[(seq[Type], Statement)]
      dispatchArguments*: seq[Statement]
    of skSequence:
      sequence*: seq[Statement]
    of skVariableGet:
      variableGetIndex*: int
    of skVariableSet:
      variableSetIndex*: int
      variableSetValue*: Statement
    of skArmStack:
      armStackFunction*: Statement
      armStackCaptures*: seq[tuple[index, valueIndex: int]]
        ## list of (variable in function stack, variable in local stack)
        ## only used for passing captures so the value is just a variable index
    of skIf:
      ifCond*, ifTrue*, ifFalse*: Statement
    of skWhile:
      whileCond*, whileBody*: Statement
    of skDoUntil:
      doUntilCond*, doUntilBody*: Statement
    of skEmitEffect:
      effect*: Statement
    of skHandleEffect:
      effectHandler*, effectBody*: Statement
    of skTuple, skList, skSet:
      elements*: seq[Statement]
    of skTable:
      entries*: seq[tuple[key, value: Statement]]
    of skGetIndex:
      getIndexAddress*: Statement
      getIndex*: int
    of skSetIndex:
      setIndexAddress*: Statement
      setIndex*: int
      setIndexValue*: Statement
    of skUnaryInstruction:
      unaryInstructionKind*: UnaryInstructionKind
      unary*: Statement
    of skBinaryInstruction:
      binaryInstructionKind*: BinaryInstructionKind
      binary1*, binary2*: Statement
  Statement* = ref StatementObj
  
  Variable* = ref object
    id*: VariableId
    name*: string
    nameHash*: Hash
    hidden*: bool # unable to look up
    knownType*: Type
    stackIndex*: int
    scope* {.cursor.}: Scope
    genericParams*: seq[TypeParameter]
      # XXX (2) maybe make this a tuple type too with signature for named and default generic params
    lazyExpression*: Expression
    evaluated*: bool

  StackSlot* = object
    kind*: VariableReferenceKind
    variable*: Variable
    value*: Value

  Context* = ref object
    ## current module or function
    origin*: Scope
      ## context closure is defined in
    captures*: Table[Variable, int]
    top*: Scope
    stackSlots*: seq[StackSlot] ## should not shrink
  
  Scope* = ref object
    ## restricted subset of variables in a context
    parent*: Scope
    context*: Context
    imports*: seq[Scope]
    variables*: seq[Variable] ## should not shrink

  VariableReferenceKind* = enum
    Local, Argument, Constant, Capture

  VariableReference* = object
    variable*: Variable
    `type`*: Type ## must have a known type
    case kind*: VariableReferenceKind
    of Local, Argument, Constant: discard
    of Capture:
      captureIndex*: int

static:
  doAssert sizeof(Value) <= 2 * sizeof(int)

proc isNoType*(t: Type): bool = t.kind == tyNoType
proc isNoType*(vt: Box[Type]): bool = vt.isNil or vt.unbox.isNoType
template tupleValue*(v: Value): untyped =
  v.arrayValue

import ./primitiveprocs
export primitiveprocs
