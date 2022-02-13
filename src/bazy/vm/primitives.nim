import std/[tables, sets, hashes], "."/[arrays, pointertag], ../language/expressions

# type system should exist for both static and runtime dispatch
# runtime-only types should only be subtypes of static-only types

type
  ValueKind* = enum
    vkNone
      ## some kind of null value
    vkInteger, vkUnsigned, vkFloat
      ## size is implementation detail
      ## same as pointer size for now but may be 32 or 48 bits
      ## if values use pointer packing
    vkList
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
    vkUnique
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
    vkSet
    vkTable
    # could be made tuples, but native macros etc need the efficiency
    vkExpression
    vkStatement
    vkScope
    # bigints can be added

  Unique*[T] = object
    id*: uint
    value*: T

  RuntimePropertyObj* = object
    properties*: HashSet[Value]
    value*: Value

  ValueObj* = object # could be cyclic
    # entire thing can be pointer tagged, but would need GC hooks
    # todo: interning for some pointer types
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: int
    of vkUnsigned:
      unsignedValue*: uint
    of vkFloat:
      floatValue*: float
    of vkList:
      listValue*: ref seq[Value]
    of vkString:
      stringValue*: (when true: ShortArray[char] else: ref string)
    of vkTuple:
      tupleValue*: ref Array[Value]
    of vkShortTuple:
      shortTupleValue*: ShortArray[Value]
    of vkReference:
      referenceValue*: ref Value
    of vkUnique:
      uniqueValue*: ref Unique[Value]
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

  TypeKind* = enum
    # maybe add unknown type for values with unknown type at runtime
    # concrete
    tyNoneValue,
    tyInteger, tyUnsigned, tyFloat,
    tyFunction, tyTuple,
    tyReference,
    tyList,
    tyString,
    tySet,
    tyTable,
    tyExpression, tyStatement, tyScope,
    tyComposite,
    tyUnique, # erased distinct type
    tyType,
    # typeclass
    tyAny, tyNone,
    tyUnion, tyIntersection, tyNot,
    tyBaseType,
    tyWithProperty,
    # matcher
    tyCustomMatcher
    # maybe add parametrized types as a typeclass
  
  Type* {.acyclic.} = object # could be cyclic
    properties*: HashSet[Value]
    case kind*: TypeKind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat,
      tyString, tyExpression, tyStatement, tyScope,
      tyAny, tyNone:
      discard
    of tyFunction:
      # arguments and return type are not reflected at runtime
      # instead function could be an atom that takes a list of any values
      # and it can have a property of its arguments and return type
      arguments*: ref seq[Type]
      # having multiple non-ref seq fields makes the compiler abort compilation for some reason
      returnType*: ref Type
    of tyTuple:
      elements*: ref seq[Type]
    of tyReference, tyList, tySet:
      elementType*: ref Type
    of tyTable:
      keyType*, valueType*: ref Type
    of tyComposite:
      fields*: Table[string, Type]
    of tyUnique:
      name*: string
      uniqueType*: ref Unique[Type]
    of tyType:
      typeValue*: ref Type
    of tyUnion, tyIntersection:
      operands*: ref seq[Type]
    of tyNot:
      notType*: ref Type
    of tyBaseType:
      baseKind*: TypeKind
    of tyWithProperty:
      typeWithProperty*: ref Type
      withProperty*: Value
    of tyCustomMatcher:
      typeMatcher*: proc (t: Type): bool
      valueMatcher*: proc (v: Value): bool
      #compare*: proc (otherMatcher, t: Type): bool
        # if closer to `t` than `otherMatcher`, return true

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
    # collection
    BuildTuple
    BuildList
    BuildSet
    BuildTable

  Instruction* {.acyclic.} = ref object
    case kind*: InstructionKind
    of NoOp: discard
    of Constant:
      constantValue*: Value
    of FunctionCall:
      function*: Instruction # evaluates to Function or native function
      arguments*: Array[Instruction]
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

  InstructionObj = typeof(Instruction()[])
  
  StatementKind* = enum
    skNone
    skConstant
    skFunctionCall
    skSequence
    # stack
    skVariableGet
    skVariableSet
    skFromImportedStack
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
  
  Variable* = ref object
    name*: string
    cachedType*: Type
    stackIndex*: int
    scope*: Scope
    lazyExpression*: Expression
    evaluated*: bool

  Context* = ref object
    ## current module or function
    imports*: seq[Context]
    #stackSize*: int
    stack*: Stack
    top*: Scope
    allVariables*: seq[Variable] ## should not shrink
  
  Scope* = ref object
    ## restricted subset of variables in a context
    #imports*: seq[Scope]
    parent*: Scope
    context*: Context
    variables*: seq[Variable] ## should not shrink
    #accumStackSize*: int
    #  ## the stack size from before the start of this scope
  
  VariableAddress* = object
    ## address of variable relative to context
    indices*: seq[int]

  VariableReference* = object
    variable*: Variable
    address*: VariableAddress

static:
  doAssert sizeof(Value) <= 2 * sizeof(int)

const
  allTypeKinds* = {low(TypeKind)..high(TypeKind)}
  concreteTypeKinds* = {tyNoneValue..tyType}
  typeclassTypeKinds* = {tyAny..tyWithProperty}
  matcherTypeKinds* = typeclassTypeKinds + {tyCustomMatcher}
  atomicTypes* = {tyNoneValue, tyInteger, tyUnsigned, tyFloat,
    tyString, tyExpression, tyStatement, tyScope}

proc get*(stack: Stack, index: int): lent Value {.inline.} =
  stack.stack[index]
proc set*(stack: Stack, index: int, value: sink Value) {.inline.} =
  stack.stack[index] = value

import macrocache

proc unique*[T](x: sink T): Unique[T] {.gcsafe.} =
  const uniqueIdCounter = CacheCounter"bazy.vm.unique"
  when nimvm:
    result.id = uniqueIdCounter.value.uint
    inc uniqueIdCounter
  else:
    var counter {.global.}: uint = static(uniqueIdCounter.value.uint)
    result.id = counter
    inc counter
  result.value = x

template makeType*(name): Type = Type(kind: `ty name`)

let
  Template* = unique Value(kind: vkNone)
  TypedTemplate* = unique Value(kind: vkNone)

proc shallowRefresh*(stack: Stack): Stack =
  result = Stack(imports: stack.imports, stack: newArray[Value](stack.stack.len))
  for i in 0 ..< stack.stack.len:
    result.stack[i] = stack.stack[i]

proc `$`*(arr: Array[char] | ShortArray[char]): string =
  result = newString(arr.len)
  for i in 0 ..< result.len:
    result[i] = arr[i]

template toRef*[T](x: T): ref T =
  var res: ref T
  new(res)
  res[] = x
  res

import ../util/objects

template mix(x) =
  mixin hash
  result = result !& hash(x)

when false:
  proc hash*(r: ref): Hash =
    mix hash(r[])
    result = !$ result

proc hash*[T](p: Unique[T]): Hash =
  mix p.id
  result = !$ result

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

proc `==`*[T](p1, p2: Unique[T]): bool {.inline.} =
  p1.id == p2.id

defineRefEquality Unique

when false:
  proc `==`*(a, b: ref): bool =
    (a.isNil and b.isNil) or (not a.isNil and not b.isNil and a[] == b[])

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