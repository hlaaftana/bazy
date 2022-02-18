import "."/[primitives, pointertag, arrays], ../language/expressions, std/[sets, tables]

proc `$`*(value: Value): string =
  case value.kind
  of vkNone: "()"
  of vkInteger: $value.integerValue
  of vkBoolean: $bool(value.integerValue)
  of vkUnsigned: $value.unsignedValue
  of vkFloat: $value.floatValue
  of vkList: ($value.listValue[])[1..^1]
  of vkString: value.stringValue[]
  of vkArray:
    var s = ($value.tupleValue.unref)[1..^1]
    s[0] = '('
    s[^1] = ')'
    s
  of vkReference: ($value.referenceValue[])
  of vkComposite:
    var s = "("
    for k, v in value.compositeValue[]:
      if s.len != len"(":
        s.add(", ")
      s.add(k)
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

template withkind(k, val): Value =
  Value(kind: `vk k`, `k Value`: val)
template withkindrefv(vk, kv, val) =
  result = Value(kind: `vk`)
  new(result.`kv`)
  result.`kv`[] = val
template withkindref(k, val) =
  withkindrefv(`vk k`, `k Value`, val)

proc toValue*(x: int): Value = withkind(integer, x)
proc toValue*(x: uint): Value = withkind(unsigned, x)
proc toValue*(x: float): Value = withkind(float, x)
proc toValue*(x: bool): Value = Value(kind: vkBoolean, integerValue: int(x))
proc toValue*(x: sink seq[Value]): Value = withkindref(list, x)
proc toValue*(x: sink string): Value = withkindref(string, x)
proc toValue*(x: sink Array[Value]): Value = Value(kind: vkArray, tupleValue: toArrayRef x.toOpenArray(0, x.len - 1))
proc toValue*(x: Type): Value = withkindrefv(vkType, typeValue, x)
proc toValue*(x: sink HashSet[Value]): Value = withkindref(set, x)
proc toValue*(x: sink Table[Value, Value]): Value = withkindref(table, x)
proc toValue*(x: sink Table[string, Value]): Value = withkindref(composite, x)
proc toValue*(x: proc (args: openarray[Value]): Value {.nimcall.}): Value = withkind(nativeFunction, x)
proc toValue*(x: Function): Value = withkind(function, x)
proc toValue*(x: Expression): Value = withkind(expression, x)
proc toValue*(x: Statement): Value = withkind(statement, x)
proc toValue*(x: Scope): Value = withkind(scope, x)

proc toType*(x: Value): Type =
  case x.kind
  of vkNone: result = Ty(NoneValue)
  of vkInteger: result = Ty(Integer)
  of vkUnsigned: result = Ty(Unsigned)
  of vkFloat: result = Ty(Float)
  of vkBoolean: result = Ty(Boolean)
  of vkList: result = Type(kind: tyList, elementType: toRef(x.listValue[][0].toType))
  of vkString: result = Ty(String)
  of vkExpression: result = Ty(Expression)
  of vkStatement: result = Ty(Statement)
  of vkScope: result = Ty(Scope)
  of vkArray:
    let val = x.tupleValue.unref
    result = Type(kind: tyTuple, elements: toRef(newSeq[Type](val.len)))
    for i in 0 ..< x.tupleValue.unref.len:
      result.elements[][i] = val[i].toType
  of vkReference:
    result = Type(kind: tyReference, elementType: toRef(x.referenceValue[].toType))
  of vkComposite:
    let val = x.compositeValue[]
    result = Type(kind: tyComposite, fields: initTable[string, Type](val.len))
    for k, v in val:
      result.fields[k] = v.toType
  of vkPropertyReference:
    result = toType(x.propertyRefValue.value)
    result.properties = x.propertyRefValue.properties
  of vkType:
    result = Type(kind: tyType, typeValue: x.typeValue)
  of vkFunction, vkNativeFunction:
    result = Ty(Function) # XXX (4) no signature
  of vkEffect:
    result = x.effectValue[].toType # XXX do what here
  of vkSet:
    result = Ty(Set)
    for v in x.setValue[]:
      result.elementType = toRef(v.toType)
      break
  of vkTable:
    result = Ty(Table)
    for k, v in x.tableValue[]:
      result.keyType = toRef(k.toType)
      result.valueType = toRef(v.toType)
      break

proc copy*(value: Value): Value =
  case value.kind
  of vkNone, vkInteger, vkBoolean, vkUnsigned, vkFloat,
    vkReference, vkType, vkFunction, vkNativeFunction: value
  of vkList:
    var newSeq = newSeq[Value](value.listValue[].len)
    for i in 0 ..< newSeq.len:
      newSeq[i] = copy value.listValue[][i]
    toValue(newSeq)
  of vkString: toValue(value.stringValue[])
  of vkArray:
    var newArray = newArray[Value](value.tupleValue.unref.len)
    for i in 0 ..< newArray.len:
      newArray[i] = copy value.tupleValue.unref[i]
    toValue(newArray)
  of vkComposite:
    var newTable = newTable[string, Value](value.compositeValue[].len)
    for k, v in value.compositeValue[]:
      newTable[k] = copy v
    Value(kind: vkComposite, compositeValue: newTable)
  of vkPropertyReference, vkEffect,
    vkSet, vkTable, vkExpression, vkStatement, vkScope:
    # unimplemented
    value

proc fromValueObj*(v: ValueObj): PointerTaggedValue =
  when pointerTaggable:
    template fromPtr(name): untyped =
      cast[pointer](v.`name Value`).tagPointer(v.kind.uint16)
    result = PointerTaggedValue:
      case v.kind
      of vkNone: 0'u64
      of vkInteger, vkBoolean: (v.kind.uint64 shl 48) or int32(v.integerValue).uint64
      of vkUnsigned: (v.kind.uint64 shl 48) or int32(v.unsignedValue).uint64
      of vkFloat: (v.kind.uint64 shl 48) or int32(v.floatValue).uint64
      of vkList: fromPtr list
      of vkString: fromPtr string
      of vkArray: cast[pointer](v.tupleValue).tagPointer(v.kind.uint16)
      of vkReference: fromPtr reference
      of vkComposite: fromPtr composite
      of vkPropertyReference: fromPtr propertyRef
      of vkType: fromPtr type
      of vkNativeFunction: fromPtr nativeFunction
      of vkFunction: fromPtr function
      of vkEffect: fromPtr effect
      of vkSet: fromPtr set
      of vkTable: fromPtr table
      of vkExpression: fromPtr expression
      of vkStatement: fromPtr statement
      of vkScope: fromPtr scope
  else:
    v.PointerTaggedValue

proc toValueObj*(p: PointerTaggedValue): ValueObj =
  when pointerTaggable:
    let val = p.uint64
    result.kind = ValueKind(val shr 48)
    template castPointer(name) =
      result.`name Value` = cast[typeof(result.`name Value`)](untagPointer(val))
    case result.kind
    of vkNone: discard
    of vkInteger, vkBoolean: result.integerValue = (val and high(uint32).uint64).int32.int
    of vkUnsigned: result.unsignedValue = (val and high(uint32).uint64).uint
    of vkFloat: result.floatValue = (val and high(uint32).uint64).float32.float
    of vkList: castPointer list
    of vkString: castPointer string
    of vkArray: result.tupleValue = cast[typeof(result.tupleValue)](untagPointer(val))
    of vkReference: castPointer reference
    of vkComposite: castPointer composite
    of vkPropertyReference: castPointer propertyRef
    of vkType: castPointer type
    of vkNativeFunction: castPointer nativeFunction
    of vkFunction: castPointer function
    of vkEffect: castPointer effect
    of vkSet: castPointer set
    of vkTable: castPointer table
    of vkExpression: castPointer expression
    of vkStatement: castPointer statement
    of vkScope: castPointer scope
  else:
    p.ValueObj
