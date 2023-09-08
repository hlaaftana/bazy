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
template unref*[T](x: ref T): untyped = x[]
template unref*[T](x: Box[T]): untyped = x.unbox
template unref*[T](x: T): untyped = x

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
    vkBoxed # XXX (4) maybe do BoxedInt32, BoxedUint32 etc
    vkInt64, vkUint64, vkFloat64
    vkType
      ## type value
    vkArray
      ## like java array but typed like TS, implementation of tuples
    vkString
      ## general byte sequence type
    vkList
      ## both seq and string are references to save memory
    vkSet
    vkTable
    vkFunction
      ## function
    vkNativeFunction
      ## Nim function that takes values as argument
    # could be pointers or serialized but for now this is more efficient:
    vkExpression
    vkStatement
    vkScope
    # bigints can be added

const boxedValueKinds* = {vkBoxed..high(ValueKind)}

type
  FullValueObj* = object
    `type`*: ref Type
      # XXX (4) actually use and account for this without losing performance
      # for now it's mostly seen, but nothing initializes values with it 
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
      referenceValue*: ref FullValueObj
    of vkBoxed:
      boxedValue*: FullValue
    of vkInt64:
      int64Value*: int64
    of vkUint64:
      uint64Value*: uint64
    of vkFloat64:
      float64Value*: float64
    of vkType:
      typeValue*: Type
    of vkArray:
      # XXX pointer field location should be same as vkList, vkString
      tupleValue*: Array[Value]
    of vkString:
      # XXX pointer field location should be same as vkArray, vkList
      stringValue*: string
    of vkList:
      # XXX pointer field location should be same as vkArray, vkString
      listValue*: seq[Value]
    of vkSet:
      setValue*: HashSet[Value]
    of vkTable:
      tableValue*: Table[Value, Value]
    of vkFunction:
      functionValue*: TreeWalkFunction
    of vkNativeFunction:
      nativeFunctionValue*: proc (args: openarray[Value]): Value {.nimcall.}
    of vkExpression:
      expressionValue*: Expression
    of vkStatement:
      statementValue*: Statement
    of vkScope:
      scopeValue*: Scope
  FullValue* = ref FullValueObj

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
      referenceValue*: ref FullValueObj
    of boxedValueKinds:
      boxedValue*: FullValue
  
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
    tyTuple, # XXX (2) make into tyComposite, tuple, named tuple, array (i.e. int^20) all at once
    # typeclass
    tyAny, tyNone, ## none is bottom type
    tyUnion, tyIntersection, tyNot,
    tyWithProperty, # XXX unused
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
  
  ParameterInstantiation* = Table[TypeParameter, Type]

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
    ntyContravariant

  TypeBase* = ref object
    # XXX (3) check arguments at generic fill time
    id*: TypeBaseId
    name*: string
    nativeType*: NativeType
    arguments*: seq[TypeParameter]
    typeMatcher*: proc (pattern, t: Type): TypeMatch
    valueMatcher*: proc (v: Value, thisType: Type): bool
    # not sure if these are temporary:
    genericMatcher*: proc (pattern: Type, t: Type, table: var ParameterInstantiation, variance = Covariant)
    genericFiller*: proc (pattern: var Type, table: ParameterInstantiation)

  TypeParameter* = ref object
    id*: TypeParameterId # this needs to be assigned
    name*: string
    bound*: TypeBound
  
  Type* = object
    # XXX (3) for easier generics etc maybe just have a base type, argument types, and properties
    properties*: Table[TypeBase, Type]
      # can be a multitable later on
    case kind*: TypeKind
    of tyNoType, tyAny, tyNone: discard
    of tyCompound:
      # XXX (7) seq might cause performance drop, add tySingleCompound, tyDoubleCompound etc
      # or optimize Array like that and use it
      base*: TypeBase
      baseArguments*: seq[Type]
    of tyTuple:
      elements*: seq[Type]
      varargs*: Box[Type] # for now only trailing
        # XXX either move to property, or allow non-trailing
      elementNames*: Table[string, int]
      unorderedFields*: Table[string, Type]
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: Box[Type]
    of tyWithProperty:
      typeWithProperty*: Box[Type]
      withProperty*: TypeBase
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
    imports*: Array[Stack]
    stack*: Array[Value]

  TreeWalkFunction* = object
    stack*: Stack
      ## persistent stack
      ## gets shallow copied when function is run
    instruction*: Instruction

  InstructionKind* = enum
    NoOp
    Constant
    FunctionCall, Dispatch
    Sequence
    # stack
    VariableGet, VariableSet
    GetAddress, SetAddress
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
    # unary
    NegInt, NegFloat
  
  BinaryInstructionKind* = range[AddInt .. DivFloat]
  UnaryInstructionKind* = range[NegInt .. NegFloat]

  InstructionObj* {.acyclic.} = object ## compact version of Statement
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
    of GetAddress:
      getAddress*: Array[int]
    of SetAddress:
      setAddress*: Array[int]
      setAddressValue*: Instruction
    of ArmStack:
      armStackFunction*: Instruction
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
  
  VariableAddress* = object
    ## address of variable relative to context
    indices*: seq[int]
  
  StatementKind* = enum
    skNone
    skConstant
    skFunctionCall, skDispatch
    skSequence
    # stack
    skVariableGet, skVariableSet
      # XXX (1) should go, closure variables should be loaded to local stack with ArmStack
    skGetAddress, skSetAddress
      # XXX (1) remove, only use above
    skArmStack
      # XXX (1) should load closure captures
    # goto
    skIf, skWhile, skDoUntil
    # effect, can emulate goto
    skEmitEffect, skHandleEffect
    # collections
    skTuple, skList, skSet, skTable
    skGetIndex, skSetIndex
    # custom instructions
    skUnaryInstruction, skBinaryInstruction

  StatementObj* {.acyclic.} = object
    ## typed/compiled expression
    # XXX differentiate between validated and unvalidated,
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
      dispatchees*: seq[(seq[Type], Statement)]
      dispatchArguments*: seq[Statement]
    of skSequence:
      sequence*: seq[Statement]
    of skVariableGet:
      variableGetIndex*: int
    of skVariableSet:
      variableSetIndex*: int
      variableSetValue*: Statement
    of skGetAddress:
      getAddress*: VariableAddress
    of skSetAddress:
      setAddress*: VariableAddress
      setAddressValue*: Statement
    of skArmStack:
      armStackFunction*: Statement
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
    name*: string
    nameHash*: Hash
    knownType*: Type
    stackIndex*: int
    scope*: Scope
    genericParams*: seq[TypeParameter]
      # XXX (3) maybe make this a tuple type too with signature for named and default generic params
    lazyExpression*: Expression
    evaluated*: bool

  Context* = ref object
    ## current module or function
    imports*: seq[Context]
      # XXX (1) imports should not work like this/exist
      # modules should create a module object with all exported variables
      # the imported variables are like closure captures from the other module
      # before a module is run, imported variables should be loaded into its stack by the module graph
      # stacks should never persist, persistent memory should be vkReference
      # static code should still run exactly like normal code, static memory
      # can still use vkReference which will be re-allocated at runtime with
      # the final static value of the reference
      # values not accessed from a vkReference always have value semantics and are internally immutable
      # comments might be outdated:
        # for now keep this but use it less
        # register memory can still exist as well as constant memory that gets inlined
    parent*: Context
      # XXX (1) context closure is defined in
    captures*: seq[VariableReference] # XXX (1)
    stack*: Stack
    stackSize*: int
    top*: Scope
    allVariables*: seq[Variable] ## should not shrink
  
  Scope* = ref object
    ## restricted subset of variables in a context
    #imports*: seq[Scope] # maybe add blacklist
    parent*: Scope
    context*: Context
    variables*: seq[Variable] ## should not shrink

  VariableReference* = object
    variable*: Variable
    `type`*: Type ## must have a known type
    address*: VariableAddress

static:
  doAssert sizeof(Value) <= 2 * sizeof(int)

proc isNoType*(t: Type): bool = t.kind == tyNone
proc isNoType*(vt: Box[Type]): bool = vt.isNil or vt.unbox.isNoType

import ./primitiveprocs
export primitiveprocs
