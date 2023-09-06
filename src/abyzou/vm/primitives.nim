import std/[tables, sets, hashes], "."/[arrays, pointertag, ids], ../language/expressions, ../util/box
export box, unbox

# type system should exist for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

type
  ValueKind* = enum
    vkNone
      ## some kind of null value
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

  #OpaqueValue* = ref object of RootObj 

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
    tyNone,
    # concrete
    tyCompound,
    tyNoneValue,
    tyInt32, tyUint32, tyFloat32, tyBool,
    tyInt64, tyUint64, tyFloat64,
    tyReference,
    tyTuple, # XXX (2) make into tyComposite, tuple, named tuple, array (i.e. int^20) all at once
    tyFunction,
    tyList,
    tyString,
    tySet,
    tyTable,
    tyExpression, tyStatement, tyScope,
    tyType,
    # typeclass
    tyAny,
    tyUnion, tyIntersection, tyNot,
    tyBaseType,
    tyWithProperty,
    tyBase,
    tySomeValue,
    # generic parameter
    tyParameter,
    # value container
    tyValue
    
  TypeMatch* = enum
    # in order of strength
    tmUnknown, tmNone,
    tmFiniteFalse, tmFalse,
    tmTrue, tmFiniteTrue,
    tmSimilar, tmGeneric,
    tmAlmostEqual, tmEqual

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
    # XXX generics?
    id*: TypeBaseId
    name*: string
    nativeType*: NativeType
    arguments*: seq[TypeParameter]
    typeMatcher*: proc (pattern, t: Type): TypeMatch
    valueMatcher*: proc (v: Value, thisType: Type): bool
    # XXX (6) maybe add compare
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
    of tyCompound:
      base*: TypeBase
      baseArguments*: seq[Type]
    of tyNoneValue,
      tyInt32, tyUint32, tyFloat32, tyBool,
      tyInt64, tyUint64, tyFloat64,
      tyString, tyExpression, tyStatement, tyScope,
      tyAny, tyNone:
      discard
    of tyTuple:
      elements*: seq[Type]
      varargs*: Box[Type] # for now only trailing
        # XXX either move to property, or allow non-trailing
      elementNames*: Table[string, int]
      unorderedFields*: Table[string, Type]
    of tyFunction:
      # XXX (2) account for Fields and Defaults property of the `arguments` tuple type
      # only considered at callsite like nim, no semantic value
      # meaning this is specific to function type relation
      arguments*: Box[Type] # tuple type, includes varargs
      returnType*: Box[Type]
    of tyReference, tyList, tySet:
      elementType*: Box[Type]
    of tyTable:
      keyType*, tableValueType*: Box[Type]
    of tyType:
      typeValue*: Box[Type]
    of tyUnion, tyIntersection:
      operands*: seq[Type]
    of tyNot:
      notType*: Box[Type]
    of tyBaseType:
      baseKind*: TypeKind
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
    #of tyGeneric:
    #  parameters*: Table[TypeParameter, TypeBound]
    #  genericPattern*: ref Type

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
    FunctionCall
    Dispatch
    Sequence
    # stack
    VariableGet
    VariableSet
    GetAddress
    SetAddress
    ArmStack
    # goto
    If
    While
    DoUntil
    # effect, can emulate goto
    EmitEffect
    HandleEffect
    # collection
    BuildTuple
    BuildList
    BuildSet
    BuildTable
    GetIndex
    SetIndex
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
    skFunctionCall
    skDispatch
    skSequence
    # stack
    skVariableGet
    skVariableSet
    skGetAddress
      # XXX (1) should go, closure variables should be loaded to local stack with ArmStack
    skSetAddress # XXX (1) remove, only use skVariableSet
    skArmStack
      # XXX (1) should load closure variables too
    # goto
    skIf
    skWhile
    skDoUntil
    # effect, can emulate goto
    skEmitEffect
    skHandleEffect
    # collections
    skTuple
    skList
    skSet
    skTable
    skGetIndex
    skSetIndex
    # custom instructions
    skUnaryInstruction
    skBinaryInstruction

  StatementObj* {.acyclic.} = object
    ## typed/compiled expression
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
    # XXX (3) maybe make this a tuple type too with signature for named and default generic params
    genericParams*: seq[TypeParameter]
    lazyExpression*: Expression
    evaluated*: bool

  Context* = ref object
    ## current module or function
    imports*: seq[Context]
      # XXX (1) imports should not work like this/exist
      # modules should probably be like JS or lua where
      # the module creates a module object which is what gets imported
      # stacks should never persist, persistent memory should be vkReference
      # static code should still run exactly like normal code, static memory
      # can still use vkReference which will be re-allocated at runtime with
      # the final static value of the reference
      # values not accessed from a vkReference always have value semantics and are internally immutable
      # comments might be outdated:
        # for now keep this but use it less
        # register memory can still exist as well as constant memory that gets inlined
        # maybe vkReference shouldn't exist and Value should always have value semantics
        # for serialization and initialization in bytecode etc
        # or just have a rule to allocate a new reference every time a vkReference constant is loaded
    parent*: Context # XXX (1)
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

proc get*(stack: Stack, index: int): lent Value {.inline.} =
  stack.stack[index]
proc set*(stack: Stack, index: int, value: sink Value) {.inline.} =
  stack.stack[index] = value

template Ty*(name): Type = Type(kind: `ty name`)

proc shallowRefresh*(stack: Stack): Stack =
  result = Stack(imports: stack.imports)
  var newStack = newArray[Value](stack.stack.len)
  for i in 0 ..< stack.stack.len:
    newStack[i] = stack.stack[i]
  result.stack = newStack

template toRef*[T](x: T): ref T =
  var res: ref T
  new(res)
  res[] = x
  res

template unref*[T](x: ref T): untyped = x[]
template unref*[T](x: Box[T]): untyped = x.unbox
template unref*[T](x: T): untyped = x

proc isNone*(t: Type): bool = t.kind == tyNone
proc isNone*(vt: Box[Type]): bool = vt.isNil or vt.unbox.isNone

import skinsuit/equals

template mix(x) =
  mixin hash
  result = result !& hash(x)

template idObject[T: ref](t: type T) {.dirty.} =
  proc hash*(p: t): Hash {.noSideEffect.} =
    if not p.isNil:
      mix p.id
    else:
      mix pointer(nil)
    result = !$ result

  proc `==`*(a, b: t): bool = a.isNil and b.isNil or (not a.isNil and not b.isNil and a.id == b.id)

idObject(TypeBase)
idObject(TypeParameter)

proc hash*(v: FullValueObj): Hash {.noSideEffect.}
proc hash*(v: Value): Hash {.noSideEffect.}
proc hash*(v: Type): Hash {.noSideEffect.}
proc hash*(v: InstructionObj): Hash {.noSideEffect.}

template hashRefObj(T): untyped {.dirty.} =
  proc hash*(v: T): Hash {.noSideEffect.} =
    if v.isNil:
      mix 0
    else:
      mix v[]
    result = !$ result

hashRefObj(ref Value)
hashRefObj(ref Type)
hashRefObj Instruction
hashRefObj Stack

template hashObj(T): untyped {.dirty.} =
  proc hash*(v: T): Hash {.noSideEffect.} =
    for f in fields(v):
      when f is ref:
        when compiles(hash(f[])):
          if not f.isNil:
            mix f[]
          else:
            mix cast[int](cast[pointer](f))
        else:
          mix cast[int](cast[pointer](f))
      else:
        mix f
    result = !$ result

hashObj FullValueObj
hashObj Value
hashObj Type
hashObj InstructionObj

proc `==`*(a, b: FullValueObj): bool {.noSideEffect.}
proc `==`*(a, b: Value): bool {.noSideEffect.}
proc `==`*(a, b: Type): bool {.noSideEffect.}
proc `==`*(a, b: InstructionObj): bool {.noSideEffect.}
proc `==`*(a, b: StatementObj): bool {.noSideEffect.}

equals *(ref Value)
equals *(ref FullValueObj)
equals *(ref Type)
equals *(ref InstructionObj)
equals *(ref StatementObj)

equals *Value
equals *FullValueObj
equals *Type
equals *InstructionObj
equals *StatementObj

import strutils

proc `$`*(t: TypeParameter): string {.inline.} = t.name

proc `$`*(t: Type): string

proc `$`*(vt: Box[Type]): string =
  if vt.isNil:
    "None"
  else: $vt.unbox

proc `$`*(value: Value): string

proc `$`*(value: FullValue): string =
  result = case value.kind
  of vkNone: "()"
  of vkInt32: $value.int32Value
  of vkUint32: $value.uint32Value
  of vkFloat32: $value.float32Value
  of vkBool: $value.boolValue
  of vkEffect: "Effect(" & $value.effectValue.unref & ")"
  of vkReference: "ref(" & $value.referenceValue.unref & ")"
  of vkBoxed: $value.boxedValue
  of vkInt64: $value.int64Value
  of vkUint64: $value.uint64Value
  of vkFloat64: $value.float64Value
  of vkList: ($value.listValue.unref)[1..^1]
  of vkString: value.stringValue.unref
  of vkArray:
    var s = ($value.tupleValue.unref)[1..^1]
    s[0] = '('
    s[^1] = ')'
    s
  of vkType: $value.typeValue
  of vkFunction: "<function>"
  of vkNativeFunction: "<native function>"
  of vkSet: $value.setValue
  of vkTable: $value.tableValue
  of vkExpression: $value.expressionValue[]
  of vkStatement: $value.statementValue[]
  of vkScope: $value.scopeValue[]

proc `$`*(value: Value): string =
  case value.kind
  of vkNone: "()"
  of vkInt32: $value.int32Value
  of vkUint32: $value.uint32Value
  of vkFloat32: $value.float32Value
  of vkBool: $value.boolValue
  of vkReference: "ref(" & $value.referenceValue.unref & ")"
  of vkEffect: "Effect(" & $value.effectValue.unref & ")"
  of boxedValueKinds: $value.boxedValue

proc `$`*(p: TypeBase): string {.inline.} = p.name

proc `$`*(tb: TypeBound): string

proc `$`*(t: Type): string =
  proc `$`(s: seq[Type]): string =
    for t in s:
      if result.len != 0:
        result.add(", ")
      result.add($t)
  result = case t.kind
  of tyCompound: t.base.name & "(" & $t.baseArguments & ")"
  of tyNoneValue: "NoneValue"
  of tyInt32: "Int32"
  of tyUint32: "Uint32"
  of tyFloat32: "Float32"
  of tyBool: "Bool"
  of tyInt64: "Int64"
  of tyUint64: "Uint64"
  of tyFloat64: "Float64"
  of tyString: "String"
  of tyExpression: "Expression"
  of tyStatement: "Statement"
  of tyScope: "Scope"
  of tyAny: "Any"
  of tyNone: "None"
  of tyTuple: "Tuple(" & $t.elements & (if t.unorderedFields.len == 0: "" else: " " & $t.unorderedFields) & (if t.varargs.isNone: ")" else: ", " & $t.varargs & "...)")
  of tyFunction:
    "Function(" & $t.arguments & ") -> " & $t.returnType
  of tyReference: "Reference(" & $t.elementType & ")"
  of tyList: "List(" & $t.elementType & ")"
  of tySet: "Set(" & $t.elementType & ")"
  of tyTable: "Table(" & $t.keyType & ", " & $t.tableValueType & ")"
  of tyType: "Type " & $t.typeValue
  of tyUnion: "Union(" & $t.operands & ")"
  of tyIntersection: "Intersection(" & $t.operands & ")"
  of tyNot: "Not " & $t.notType
  of tyBaseType: "BaseType " & $t.baseKind
  of tyWithProperty: "WithProperty(" & $t.typeWithProperty & ", " & $t.withProperty & ")"
  of tyBase: "Base(" & $t.typeBase & ")"
  of tySomeValue: "SomeValue(" & $t.someValueType & ")"
  of tyParameter: "Parameter(" & $t.parameter.name & ")"
  of tyValue: "Value(" & $t.value & ": " & $t.valueType & ")"
  #of tyGeneric:
  #  var s = "Generic["
  #  var i = 0
  #  for p, b in t.parameters:
  #    if i != 0: s.add(", ")
  #    else: inc i
  #    s.add(p.name & ": " & $b)
  #  s & "](" & $t.genericPattern[] & ")"
  if t.properties.len != 0:
    result.add(" {") 
    var afterFirst = false
    for tag, arg in t.properties:
      if afterFirst: result.add(", ")
      else: afterFirst = true
      result.add($arg)
      when false:
        if args.len != 0:
          result.add('(')
          for i, arg in args:
            if i != 0: result.add(", ")
            result.add($arg)
          result.add(')')
    result.add('}')

proc `$`*(tb: TypeBound): string =
  (case tb.variance
  of Covariant: '+'
  of Contravariant: '-'
  of Invariant: '~'
  of Ultravariant: '*') & $tb.boundType

proc `$`*(variable: Variable): string =
  variable.name & ": " & $variable.knownType

proc `$`*(context: Context): string =
  result = "context\n"
  for v in context.allVariables:
    result.add("  " & $v & "\n")
  result.add("imports\n")
  for c in context.imports:
    for line in splitLines($c):
      result.add("  " & line & "\n")

proc `$`*(scope: Scope): string =
  result = "scope\n"
  for v in scope.variables:
    result.add("  " & $v & "\n")
  if scope.parent.isNil:
    result.add("imports\n")
    for c in scope.context.imports:
      for line in splitLines($c):
        result.add("  " & line & "\n")
  else:
    result.add("parent ")
    for line in splitLines($scope.parent):
      result.add("  " & line & "\n")
