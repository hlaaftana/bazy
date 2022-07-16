import std/[tables, sets, hashes], "."/[arrays, pointertag], ../language/expressions

# type system should exist for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

template minimal(T): untyped =
  when sizeof(T) == sizeof(int):
    T
  else:
    ref T

type
  ValueKind* = enum
    vkNone
      ## some kind of null value
    vkInteger, vkUnsigned, vkFloat, vkBoolean
      ## size is implementation detail
      ## same as pointer size for now but may be 32 or 48 bits
      ## if values use pointer packing
    vkList
      ## both seq and string are references to save memory
    #vkShortestString
    #  ## word size string
    vkString
      ## general byte sequence type
    vkArray
      ## like java array but typed like TS, not necessarily hetero or homogenously typed
    vkReference
      ## reference (pointer) to value
    vkComposite
      ## like tuple, but fields are tied to names and unordered
      ## (really the names define the order and the names can at least be shortstrings)
    vkPropertyReference
      ## references with some type properties attached at runtime
      ## which are also runtime values
      # XXX actually use and account for this without losing performance
    vkType
      ## type value
    vkNativeFunction
      ## Nim function that takes values as argument
    vkFunction
      ## function
    vkEffect
      ## embedded effect value for exceptions/return/yield/whatever
    vkSet
    vkTable
    # could be pointers or serialized but for now this is more efficient:
    vkExpression
    vkStatement
    vkScope
    #vkOpaque
    # bigints can be added

  RuntimePropertyObj* = object
    properties*: Properties
    value*: Value
  
  CompositeNameId* = int

  #OpaqueValue* = ref object of RootObj 

  ValueObj* = object # could be cyclic
    # entire thing can be pointer tagged, but would need GC hooks
    # maybe interning for some pointer types
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger, vkBoolean:
      integerValue*: int
    of vkUnsigned:
      unsignedValue*: uint
    of vkFloat:
      floatValue*: float
    of vkList:
      listValue*: minimal seq[Value]
    of vkString:
      stringValue*: minimal string
    of vkArray:
      tupleValue*: minimal Array[Value]
    of vkReference:
      referenceValue*: ref Value
    of vkComposite:
      compositeValue*: minimal Table[CompositeNameId, Value]#ArrayRef[tuple[id: CompositeNameId, value: Value]]
      # ^ arrayref version here crashes orc compiler
      # XXX probably better than table to use seq
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
    of vkSet:
      setValue*: ref HashSet[Value]
    of vkTable:
      tableValue*: ref Table[Value, Value]
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

  PropertyTag* = ref object
    name*: string
    argumentTypes*: seq[Type]
    typeMatcher*: proc (t: Type, arguments: seq[Value]): TypeMatch
    valueMatcher*: proc (v: Value, arguments: seq[Value]): bool

  Property* = object
    tag*: PropertyTag
    arguments*: seq[Value]
  
  Properties* = object
    table*: Table[PropertyTag, seq[Value]]

  TypeKind* = enum
    # maybe add unknown type for values with unknown type at runtime
    # concrete
    tyNoneValue,
    tyInteger, tyUnsigned, tyFloat, tyBoolean,
    tyFunction, tyTuple,
    tyReference,
    tyList,
    tyString,
    tySet,
    tyTable,
    tyExpression, tyStatement, tyScope,
    tyComposite,
    tyType,
    # typeclass
    tyAny, tyNone,
    tyUnion, tyIntersection, tyNot,
    tyBaseType,
    tyWithProperty,
    tyCustomMatcher
    # XXX (0) generics: generic type + mechanism that instantiates symbols with generic types
    # only works with symbols meaning only symbol resolving logic has to deal with the instantiation
    # otherwise generic type by itself can work
    # ^ maybe remove tyGeneric and put the parameter list in Variable
    tyParameter,
    #tyGeneric

  ParameterType* = ref object
    name*: string
    bound*: TypeBound
  
  Type* {.acyclic.} = object # could be cyclic
    properties*: Properties
    case kind*: TypeKind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat, tyBoolean,
      tyString, tyExpression, tyStatement, tyScope,
      tyAny, tyNone:
      discard
    of tyFunction:
      # XXX (2) signature with argument names and default values can be a property
      # only considered at callsite like nim, no semantic value
      # argument types could go in type or part of the property (good for runtime checking)
      arguments*: ref Type # tuple type, includes varargs
      returnType*: ref Type
    of tyTuple:
      # XXX (1) maybe allow names in a property to reflect regular named tuples which would then extend to named arguments
      # maybe signature property instead, ordered names can be accessors instead of like composite
      elements*: seq[Type]
      varargs*: ref Type # for now only trailing
    of tyReference, tyList, tySet:
      elementType*: ref Type
    of tyTable:
      keyType*, valueType*: ref Type
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
    of tyWithProperty:
      typeWithProperty*: ref Type
      withProperty*: PropertyTag
    of tyCustomMatcher:
      typeMatcher*: proc (t: Type): TypeMatch
      valueMatcher*: proc (v: Value): bool
      # could add custom compare
    of tyParameter:
      parameter*: ParameterType
    #of tyGeneric:
    #  parameters*: Table[ParameterType, TypeBound]
    #  genericPattern*: ref Type
    
  TypeMatch* = enum
    # in order of strength
    tmUnknown, tmNone, tmFiniteFalse, tmFalse, tmTrue, tmFiniteTrue, tmGeneric, tmAlmostEqual, tmEqual

  Variance* = enum
    Covariant
    Contravariant
    Invariant
    Ultravariant
  
  TypeBound* = object
    boundType*: Type
    variance*: Variance

  Stack* {.acyclic.} = ref object
    imports*: Array[Stack]
    stack*: Array[Value]

  Function* = ref object
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
    FromImportedStack
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
    BuildComposite
    GetComposite
    SetComposite
    GetIndex
    SetIndex
    # binary
    AddInt, SubInt, MulInt, DivInt
    AddFloat, SubFloat, MulFloat, DivFloat
    # unary
    NegInt, NegFloat
  
  BinaryInstructionKind* = range[AddInt .. DivFloat]
  UnaryInstructionKind* = range[NegInt .. NegFloat]

  Instruction* {.acyclic.} = ref object
    case kind*: InstructionKind
    of NoOp: discard
    of Constant:
      constantValue*: Value
    of FunctionCall:
      function*: Instruction # evaluates to Function or native function
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
    of FromImportedStack:
      importedStackIndex*: int
      importedStackInstruction*: Instruction
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
    of BuildComposite:
      composite*: Array[tuple[id: CompositeNameId, value: Instruction]]
    of GetComposite:
      getComposite*: Instruction
      getCompositeId*: CompositeNameId
    of SetComposite:
      setComposite*: Instruction
      setCompositeId*: CompositeNameId
      setCompositeValue*: Instruction
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

  InstructionObj = typeof(Instruction()[])
  
  StatementKind* = enum
    skNone
    skConstant
    skFunctionCall
    skDispatch
    skSequence
    # stack
    skVariableGet
    skVariableSet
    skFromImportedStack
    skSetAddress
    skArmStack
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
    skComposite
    skGetComposite
    skSetComposite
    skGetIndex
    skSetIndex
    # custom instructions
    skUnaryInstruction
    skBinaryInstruction

  Statement* {.acyclic.} = ref object
    ## typed/compiled expression
    cachedType*: Type
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
    of skFromImportedStack:
      importedStackIndex*: int
      importedStackStatement*: Statement
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
    of skComposite:
      composite*: seq[tuple[key: string, value: Statement]]
    of skGetComposite:
      getComposite*: Statement
      getCompositeName*: string
    of skSetComposite:
      setComposite*: Statement
      setCompositeName*: string
      setCompositeValue*: Instruction
    of skGetIndex:
      getIndexAddress*: Statement
      getIndex*: int
    of skSetIndex:
      setIndexAddress*: Statement
      setIndex*: int
      setIndexValue*: Statement
    of skUnaryInstruction:
      unaryInstructionKind*: UnaryInstructionKind
      unary*: Instruction
    of skBinaryInstruction:
      binaryInstructionKind*: BinaryInstructionKind
      binary1*, binary2*: Instruction
  
  ParameterInstantiation* = Table[ParameterType, Type]
  
  Variable* = ref object
    name*: string
    cachedType*: Type
    stackIndex*: int
    scope*: Scope
    genericParams*: seq[ParameterType]
    lazyExpression*: Expression
    evaluated*: bool

  Context* = ref object
    ## current module or function
    imports*: seq[Context]
    stack*: Stack
    top*: Scope
    allVariables*: seq[Variable] ## should not shrink
  
  Scope* = ref object
    ## restricted subset of variables in a context
    #imports*: seq[Scope] # maybe add blacklist
    parent*: Scope
    context*: Context
    variables*: seq[Variable] ## should not shrink
  
  VariableAddress* = object
    ## address of variable relative to context
    indices*: seq[int]

  VariableReference* = object
    variable*: Variable
    address*: VariableAddress

static:
  doAssert sizeof(Value) <= 2 * sizeof(int)

var
  compositeNameIdTable*: Table[string, CompositeNameId]
  compositeNames*: seq[string]

proc getCompositeNameId*(name: string): CompositeNameId =
  compositeNameIdTable.withValue(name, id):
    result = id[]
  do:
    result = CompositeNameId(compositeNames.len)
    compositeNameIdTable[name] = result
    compositeNames.add(name)

proc getCompositeName*(id: CompositeNameId): string =
  compositeNames[id.int]

when false:
  import algorithm

  proc toCompositeArray*[T](table: Array[tuple[key: string, value: T]]): Array[tuple[id: CompositeNameId, value: T]] =
    result = newArray[typeof result[0]](table.len)
    for i, (k, v) in table:
      let id = k.getCompositeNameId
      result[i] = (id, v)
    seq[typeof result[0]](result).sort(proc (a, b: auto): int = cmp(a[0], b[0]))

  proc toCompositeArray*[T](table: Table[string, T]): Array[tuple[id: CompositeNameId, value: T]] =
    result = newArray[typeof result[0]](table.len)
    var i = 0
    for k, v in table:
      let id = k.getCompositeNameId
      result[i] = (id, v)
      inc i
    seq[typeof result[0]](result).sort(proc (a, b: typeof(result[0])): int = cmp(a[0], b[0]))

  proc get*[T](arr: Array[tuple[id: CompositeNameId, value: T]], id: CompositeNameId): T =
    arr[binarySearch(seq[typeof arr[0]](arr), id,
      proc (item: typeof(arr[0]), id: CompositeNameId): int = cmp(item[0], id))][1]

  proc set*[T](arr: var Array[tuple[id: CompositeNameId, value: T]], id: CompositeNameId, val: T) =
    arr[binarySearch(seq[typeof arr[0]](arr), id,
      proc (item: typeof(arr[0]), id: CompositeNameId): int = cmp(item[0], id))][1] =
        val
else:
  import algorithm
  proc toCompositeArray*[T](table: Array[tuple[key: string, value: T]]): Array[tuple[id: CompositeNameId, value: T]] =
    result = newArray[typeof result[0]](table.len)
    for i, (k, v) in table:
      let id = k.getCompositeNameId
      result[i] = (id, v)
    seq[typeof result[0]](result).sort(proc (a, b: auto): int = cmp(a[0], b[0]))

  proc toCompositeArray*[T](table: Table[string, T]): Table[CompositeNameId, T] =
    for k, v in table:
      let id = k.getCompositeNameId
      result[id] = v

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

template unref*[T](x: T): untyped = x

import ../util/objects

template mix(x) =
  mixin hash
  result = result !& hash(x)

proc hash*(p: PropertyTag): Hash =
  mix cast[pointer](p)
  result = !$ result

proc `==`*(a, b: PropertyTag): bool = same(a, b)

proc hash*(p: ParameterType): Hash =
  mix cast[pointer](p)
  result = !$ result

proc `==`*(a, b: ParameterType): bool = same(a, b)

proc hash*(v: Value): Hash {.noSideEffect.}
proc hash*(v: Type): Hash {.noSideEffect.}
proc hash*(v: InstructionObj): Hash {.noSideEffect.}

proc hash*(v: ref Value): Hash =
  if v.isNil:
    mix 0
  else:
    mix v[]
  result = !$ result

proc hash*(v: ref Type): Hash =
  if v.isNil:
    mix 0
  else:
    mix v[]
  result = !$ result

proc hash*(v: Instruction): Hash =
  if v.isNil:
    mix 0
  else:
    mix v[]
  result = !$ result

proc hash*(v: Value): Hash =
  for f in fields(v):
    when f is ref:
      when compiles(hash(f[])):
        if v.kind notin {vkReference, vkFunction} and not f.isNil:
          mix(f[])
        else:
          mix cast[int](cast[pointer](f))
      else:
        mix cast[int](cast[pointer](f))
    else:
      mix f
  result = !$ result

proc hash*(v: Type): Hash =
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

proc hash*(v: InstructionObj): Hash =
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

proc `==`*(a, b: Value): bool {.noSideEffect.}
proc `==`*(a, b: Type): bool {.noSideEffect.}
proc `==`*(a, b: InstructionObj): bool {.noSideEffect.}
proc `==`*(a, b: typeof(Statement()[])): bool {.noSideEffect.}

defineRefEquality Value
defineRefEquality Type
defineRefEquality InstructionObj
defineRefEquality typeof(Statement()[])

proc `==`*(a, b: Value): bool =
  zipFields(a, b, aField, bField):
    when aField is ref:
      if a.kind notin {vkReference, vkFunction} and not aField.isNil and not bField.isNil:
        if aField[] != bField[]:
          return false
      else:
        if aField != bField:
          return false
    else:
      if aField != bField:
        return false
  return true

proc `==`*(a, b: Type): bool =
  zipFields(a, b, aField, bField):
    when aField is ref:
      if not aField.isNil and not bField.isNil:
        if aField[] != bField[]:
          return false
      else:
        if aField != bField:
          return false
    else:
      if aField != bField:
        return false
  return true

proc `==`*(a, b: InstructionObj): bool =
  zipFields(forceElse = true, a, b, aField, bField):
    when aField is ref:
      if not aField.isNil and not bField.isNil:
        if aField[] != bField[]:
          return false
      else:
        if aField != bField:
          return false
    else:
      if aField != bField:
        return false
  return true

proc `==`*(a, b: typeof(Statement()[])): bool =
  zipFields(a, b, aField, bField):
    when aField is ref:
      if not aField.isNil and not bField.isNil:
        if aField[] != bField[]:
          return false
      else:
        if aField != bField:
          return false
    else:
      if aField != bField:
        return false
  return true

import strutils

proc `$`*(t: Type): string

proc `$`*(value: Value): string =
  case value.kind
  of vkNone: "()"
  of vkInteger: $value.integerValue
  of vkBoolean: $bool(value.integerValue)
  of vkUnsigned: $value.unsignedValue
  of vkFloat: $value.floatValue
  of vkList: ($value.listValue.unref)[1..^1]
  of vkString: value.stringValue.unref
  of vkArray:
    var s = ($value.tupleValue.unref)[1..^1]
    s[0] = '('
    s[^1] = ')'
    s
  of vkReference: ($value.referenceValue[])
  of vkComposite:
    var s = "("
    for k, v in value.compositeValue[]:#.items:
      if s.len != len"(":
        s.add(", ")
      s.add(k.getCompositeName)
      s.add(": ")
      s.add($v)
    s.add(')')
    s
  of vkPropertyReference: $value.propertyRefValue[].value
  of vkType: $value.typeValue[]
  of vkFunction: "<function>"
  of vkNativeFunction: "<native function>"
  of vkEffect: $value.effectValue[]
  of vkSet: $value.setValue[]
  of vkTable: $value.tableValue[]
  of vkExpression: $value.expressionValue[]
  of vkStatement: $value.statementValue[]
  of vkScope: $value.scopeValue[]

proc `$`*(p: PropertyTag): string =
  p.name

proc `$`*(p: Property): string =
  result = $p.tag
  if p.arguments.len != 0:
    result.add('(')
    for i, a in p.arguments:
      if i != 0:
        result.add(", ")
      result.add($a)
    result.add(')')

proc `$`*(tb: TypeBound): string

proc `$`*(t: Type): string =
  proc `$`(s: seq[Type]): string =
    for t in s:
      if result.len != 0:
        result.add(", ")
      result.add($t)
  result = case t.kind
  of tyNoneValue: "NoneValue"
  of tyInteger: "Int"
  of tyUnsigned: "Unsigned"
  of tyFloat: "Float"
  of tyBoolean: "Boolean"
  of tyString: "String"
  of tyExpression: "Expression"
  of tyStatement: "Statement"
  of tyScope: "Scope"
  of tyAny: "Any"
  of tyNone: "None"
  of tyFunction:
    "Function(" & $t.arguments[] & ") -> " & $t.returnType[]
  of tyTuple: "Tuple(" & $t.elements & (if t.varargs.isNil: ")" else: ", " & $t.varargs[] & "...)")
  of tyReference: "Reference(" & $t.elementType[] & ")"
  of tyList: "List(" & $t.elementType[] & ")"
  of tySet: "Set(" & $t.elementType[] & ")"
  of tyTable: "Table(" & $t.keyType[] & ", " & $t.valueType[] & ")"
  of tyComposite: "Composite" & $t.fields
  of tyType: "Type " & $t.typeValue[]
  of tyUnion: "Union(" & $t.operands & ")"
  of tyIntersection: "Intersection(" & $t.operands & ")"
  of tyNot: "Not " & $t.notType[]
  of tyBaseType: "BaseType " & $t.baseKind
  of tyWithProperty: "WithProperty(" & $t.typeWithProperty[] & ", " & $t.withProperty & ")"
  of tyCustomMatcher: "Custom"
  of tyParameter: "Parameter(" & $t.parameter.name & ")"
  #of tyGeneric:
  #  var s = "Generic["
  #  var i = 0
  #  for p, b in t.parameters:
  #    if i != 0: s.add(", ")
  #    else: inc i
  #    s.add(p.name & ": " & $b)
  #  s & "](" & $t.genericPattern[] & ")"
  if t.properties.table.len != 0:
    result.add(" {") 
    for tag, args in t.properties.table:
      result.add($Property(tag: tag, arguments: args))
    result.add('}')

proc `$`*(tb: TypeBound): string =
  (case tb.variance
  of Covariant: '+'
  of Contravariant: '-'
  of Invariant: '~'
  of Ultravariant: '*') & $tb.boundType

proc `$`*(variable: Variable): string =
  variable.name & ": " & $variable.cachedType

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
