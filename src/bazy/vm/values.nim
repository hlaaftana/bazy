import "."/[primitives, pointertag, arrays], ../language/expressions, std/[sets, tables], ../util/box

template withkind(k, val): Value =
  Value(kind: `vk k`, `k Value`: val)
template withkindrefv(vk, kv, val): Value =
  Value(kind: `vk`, boxedValue: BoxedValue(kind: `vk`, `kv`: val))
template withkindref(k, val): Value =
  withkindrefv(`vk k`, `k Value`, val)
when false:
  template withkindpossibleref(k, val): untyped =
    when result.`k Value` is ref:
      withkindref(k, val)
    else:
      withkind(k, val)
template withkindboxv(vk, kv, val): untyped =
  withkindrefv(vk, kv, val)#Value(kind: `vk`, `kv`: box `val`)
template withkindbox(k, val): untyped =
  withkindboxv(`vk k`, `k Value`, val)
template withkindpossiblebox(k, val): untyped {.used.} =
  when result.`k Value` is Box:
    withkindbox(k, val)
  else:
    withkind(k, val)

proc toValue*(x: int32): Value = withkind(int32, x)
proc toValue*(x: uint32): Value = withkind(uint32, x)
proc toValue*(x: float32): Value = withkind(float32, x)
proc toValue*(x: bool): Value = withkind(bool, x)
proc toValue*(x: int64): Value = withkindbox(int64, x)
proc toValue*(x: uint64): Value = withkindbox(uint64, x)
proc toValue*(x: float64): Value = withkindbox(float64, x)
proc toValue*(x: sink seq[Value]): Value = withkindbox(list, x)
proc toValue*(x: sink string): Value = withkindbox(string, x)
proc toValue*(x: sink Array[Value]): Value =
  template arr(a: untyped): untyped =
    when result.tupleValue is ArrayRef:
      toArrayRef(a)
    else:
      toArray(a)
  withkindbox(array, arr x.toOpenArray(0, x.len - 1))
proc toValue*(x: Type): Value = Value(kind: vkType, boxedValue: BoxedValue(kind: vkType, typeValue: x))
proc toValue*(x: Box[Type]): Value = Value(kind: vkType, boxedValue: BoxedValue(kind: vkType, typeValue: x.unbox))
proc toValue*(x: sink HashSet[Value]): Value = withkindbox(set, x)
proc toValue*(x: sink Table[Value, Value]): Value = withkindbox(table, x)
proc toValue*(x: sink Table[CompositeNameId, Value]#[Array[(CompositeNameId, Value)]]#): Value = withkindbox(composite, x)
proc toValue*(x: sink Table[string, Value]): Value = toValue(toCompositeArray(x))
proc toValue*(x: proc (args: openarray[Value]): Value {.nimcall.}): Value = withkindbox(nativeFunction, x)
proc toValue*(x: Function): Value = withkindbox(function, x)
proc toValue*(x: Expression): Value = withkindbox(expression, x)
proc toValue*(x: Statement): Value = withkindbox(statement, x)
proc toValue*(x: Scope): Value = withkindbox(scope, x)

proc getType*(x: Value): Type =
  case x.kind
  of vkNone: result = Ty(NoneValue)
  of vkInt32: result = Ty(Integer)
  of vkUint32: result = Ty(Unsigned)
  of vkFloat32: result = Ty(Float)
  of vkInt64: discard # result = Ty(Int32)
  of vkUint64: discard # result = Ty(Uint32)
  of vkFloat64: discard # result = Ty(Float32)
  of vkBool: result = Ty(Boolean)
  of boxedValueKinds - {vkInt64, vkUint64, vkFloat64}: result = x.boxedValue.type[]
  of vkEffect:
    result = x.effectValue.unref.getType # XXX do what here
  when false:
    case x.kind
    of vkList: result = Type(kind: tyList, elementType: x.listValue.unref[0].getType.box)
    of vkString: result = Ty(String)
    of vkExpression: result = Ty(Expression)
    of vkStatement: result = Ty(Statement)
    of vkScope: result = Ty(Scope)
    of vkArray:
      let val = x.tupleValue.unref
      result = Type(kind: tyTuple, elements: newSeq[Type](val.len))
      for i in 0 ..< x.tupleValue.unref.len:
        result.elements[i] = val[i].getType
    of vkReference:
      result = Type(kind: tyReference, elementType: x.referenceValue[].getType.box)
    of vkComposite:
      let val = x.compositeValue.unref
      result = Type(kind: tyComposite, fields: initTable[string, Type](val.unref.len))
      for k, v in val.unref:#.items:
        result.fields[k.getCompositeName] = v.getType
    of vkPropertyReference:
      result = getType(x.propertyRefValue.unbox.value)
      result.properties = x.propertyRefValue.unbox.properties
    of vkType:
      result = Type(kind: tyType, typeValue: x.typeValue)
    of vkFunction, vkNativeFunction:
      result = Ty(Function) # XXX (2) no signature
    of vkSet:
      result = Ty(Set)
      for v in x.setValue.unbox:
        result.elementType = v.getType.box
        break
    of vkTable:
      result = Ty(Table)
      for k, v in x.tableValue.unbox:
        result.keyType = k.getType.box
        result.valueType = v.getType.box
        break

proc copy*(value: Value): Value =
  case value.kind
  of vkNone, vkInt64, vkBool, vkUint64, vkFloat64,
    vkInt32, vkFloat32, vkUint32,
    vkType, vkFunction, vkNativeFunction: value
  of vkList:
    var newSeq = newSeq[Value](value.listValue.unref.len)
    for i in 0 ..< newSeq.len:
      newSeq[i] = copy value.listValue.unref[i]
    toValue(newSeq)
  of vkString: toValue(value.stringValue.unref)
  of vkArray:
    var newArray = newArray[Value](value.tupleValue.unref.len)
    for i in 0 ..< newArray.len:
      newArray[i] = copy value.tupleValue.unref[i]
    toValue(newArray)
  of vkComposite:
    var newTable = initTable[CompositeNameId, Value](value.compositeValue.unref.len)#newArray[(CompositeNameId, Value)](value.compositeValue[].len)
    #var i = 0
    for k, v in value.compositeValue.unref:#.items:
      newTable[k] = copy v#i] = (k, copy v)
      #inc i
    Value(kind: vkComposite, compositeValue: newTable.toRef)#toValue(newTable)
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
