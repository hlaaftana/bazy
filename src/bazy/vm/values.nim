import "."/[primitives, pointertag, arrays], ../language/expressions, std/[sets, tables]

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
proc toValue*(x: sink seq[Value]): Value = withkindref(list, x)
proc toValue*(x: sink string): Value =
  when typeof(Value().stringValue) is ref:
    withkindref(string, x)
  else:
    var arr = newShortArray[char](x.len)
    for i in 0 ..< x.len:
      arr[i] = x[i]
    Value(kind: vkString, stringValue: arr)
proc toValue*(x: sink(ShortArray[char] | Array[char])): Value =
  when typeof(Value().stringValue) is ref:
    toValue($x)
  else:
    Value(kind: vkString, stringValue: x)
proc toValue*(x: sink Array[Value]): Value = withkindrefv(vkTuple, tupleValue, x.toOpenArray(0, x.len - 1).toSafeArray)
when false:
  proc toValue*(x: sink ShortArray[Value]): Value = withkindrefv(vkShortTuple, shortTupleValue, x.toOpenArray(0, x.len - 1).toSafeArray)
proc toValue*(x: Type): Value = withkindrefv(vkType, typeValue, x)
proc toValue*(x: sink HashSet[Value]): Value = withkindref(set, x)
proc toValue*(x: sink Table[Value, Value]): Value = withkindref(table, x)
proc toValue*(x: proc (args: openarray[Value]): Value {.nimcall.}): Value = withkind(nativeFunction, x)
proc toValue*(x: Function): Value = withkind(function, x)
proc toValue*(x: Expression): Value = withkind(expression, x)
proc toValue*(x: Statement): Value = withkind(statement, x)
proc toValue*(x: Scope): Value = withkind(scope, x)
proc toValue*(x: Unique[Value]): Value = withkindref(unique, x)

proc toType*(x: Value): Type =
  case x.kind
  of vkNone: result = makeType(NoneValue)
  of vkInteger: result = makeType(Integer)
  of vkUnsigned: result = makeType(Unsigned)
  of vkFloat: result = makeType(Float)
  of vkList: result = Type(kind: tyList, elementType: toRef(x.listValue[][0].toType))
  of vkString: result = makeType(String)
  of vkExpression: result = makeType(Expression)
  of vkStatement: result = makeType(Statement)
  of vkScope: result = makeType(Scope)
  of vkTuple:
    result = Type(kind: tyTuple, elements: toRef(newSeq[Type](x.tupleValue[].len)))
    for i in 0 ..< x.tupleValue[].len:
      result.elements[][i] = x.tupleValue[][i].toType
  of vkShortTuple:
    result = Type(kind: tyTuple, elements: toRef(newSeq[Type](x.shortTupleValue[].len)))
    for i in 0 ..< x.shortTupleValue[].len:
      result.elements[][i] = x.shortTupleValue[][i].toType
  of vkReference:
    result = Type(kind: tyReference, elementType: toRef(x.referenceValue[].toType))
  of vkUnique:
    result = Type(kind: tyReference, elementType: toRef(x.uniqueValue.value.toType))
  of vkComposite:
    result = Type(kind: tyComposite, fields: initTable[string, Type](x.compositeValue[].len))
    for k, v in x.compositeValue[]:
      result.fields[k] = v.toType
  of vkPropertyReference:
    result = toType(x.propertyRefValue.value)
    result.properties.incl(x.propertyRefValue.properties)
  of vkType:
    result = Type(kind: tyType, typeValue: x.typeValue)
  of vkFunction, vkNativeFunction:
    result = makeType(Function) # XXX
  of vkEffect:
    result = x.effectValue[].toType # XXX
  of vkSet:
    result = makeType(Set)
    for v in x.setValue[]:
      result.elementType = toRef(v.toType)
      break
  of vkTable:
    result = makeType(Table)
    for k, v in x.tableValue[]:
      result.keyType = toRef(k.toType)
      result.valueType = toRef(v.toType)
      break

proc fromValueObj*(v: ValueObj): PointerTaggedValue =
  when pointerTaggable:
    template fromPtr(name): untyped =
      cast[pointer](v.`name Value`).tagPointer(v.kind.uint16)
    result = PointerTaggedValue:
      case v.kind
      of vkNone: 0'u64
      of vkInteger: (v.kind.uint64 shl 48) or int32(v.integerValue).uint64
      of vkUnsigned: (v.kind.uint64 shl 48) or int32(v.unsignedValue).uint64
      of vkFloat: (v.kind.uint64 shl 48) or int32(v.floatValue).uint64
      of vkList: fromPtr list
      of vkString: fromPtr string
      of vkTuple: cast[pointer](v.tupleValue).tagPointer(v.kind.uint16)
      of vkShortTuple: fromPtr shortTuple
      of vkReference: fromPtr reference
      of vkUnique: fromPtr unique
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
    of vkInteger: result.integerValue = (val and high(uint32).uint64).int32.int
    of vkUnsigned: result.unsignedValue = (val and high(uint32).uint64).uint
    of vkFloat: result.floatValue = (val and high(uint32).uint64).float32.float
    of vkList: castPointer list
    of vkString: castPointer string
    of vkTuple: result.tupleValue = cast[typeof(result.tupleValue)](untagPointer(val))
    of vkShortTuple: castPointer shortTuple
    of vkReference: castPointer reference
    of vkUnique: castPointer unique
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
