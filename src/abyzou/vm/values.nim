import "."/[primitives, arrays], ../language/expressions, std/[sets, tables], ../util/box

template withkind(k, val): Value =
  Value(kind: `vk k`, `k Value`: val)
template withkindboxv(vk, kv, val): untyped =
  Value(kind: `vk`, boxedValue: FullValue(kind: `vk`, `kv`: val))
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
proc toValue*(x: float64): Value =
  toValue(x.float32)
  #withkindbox(float64, x)
proc toValue*(x: int): Value =
  if x <= high(int32) and x >= low(int32):
    toValue(int32(x))
  else:
    toValue(int64(x))
proc toValue*(x: uint): Value =
  if x <= high(uint32):
    toValue(uint32(x))
  else:
    toValue(uint64(x))
proc toValue*(x: sink seq[Value]): Value = withkindbox(list, x)
proc toValue*(x: sink string): Value = withkindbox(string, x)
proc toValue*(x: sink Array[Value]): Value =
  template arr(a: untyped): untyped =
    when result.boxedValue.tupleValue is ArrayRef:
      toArrayRef(a)
    else:
      toArray(a)
  withkindboxv(vkArray, tupleValue, arr x.toOpenArray(0, x.len - 1))
proc toValue*(x: Type): Value = Value(kind: vkType, boxedValue: FullValue(kind: vkType, typeValue: x))
proc toValue*(x: Box[Type]): Value = Value(kind: vkType, boxedValue: FullValue(kind: vkType, typeValue: x.unbox))
proc toValue*(x: sink HashSet[Value]): Value = withkindbox(set, x)
proc toValue*(x: sink Table[Value, Value]): Value = withkindbox(table, x)
proc toValue*(x: proc (args: openarray[Value]): Value {.nimcall.}): Value = withkindbox(nativeFunction, x)
proc toValue*(x: TreeWalkFunction): Value = withkindbox(function, x)
proc toValue*(x: Expression): Value = withkindbox(expression, x)
proc toValue*(x: Statement): Value = withkindbox(statement, x)
proc toValue*(x: Scope): Value = withkindbox(scope, x)

proc toFullValueObj*(x: Value): FullValueObj =
  case x.kind
  of vkNone: FullValueObj(kind: vkNone)
  of vkInt32: FullValueObj(kind: vkInt32, int32Value: x.int32Value)
  of vkUint32: FullValueObj(kind: vkUint32, uint32Value: x.uint32Value)
  of vkFloat32: FullValueObj(kind: vkFloat32, float32Value: x.float32Value)
  of vkBool: FullValueObj(kind: vkBool, boolValue: x.boolValue)
  of vkReference: FullValueObj(kind: vkReference, referenceValue: x.referenceValue)
  of boxedValueKinds: x.boxedValue[]
  of vkEffect: x.effectValue.unbox.toFullValueObj

proc toSmallValue*(x: FullValueObj | FullValue): Value =
  let unboxedKind = x.kind notin boxedValueKinds
  if unboxedKind and x.type.isNil:
    case x.kind
    of vkNone: result = Value(kind: vkNone)
    of vkInt32: result = Value(kind: vkInt32, int32Value: x.int32Value)
    of vkUint32: result = Value(kind: vkUint32, uint32Value: x.uint32Value) 
    of vkFloat32: result = Value(kind: vkFloat32, float32Value: x.float32Value) 
    of vkBool: result = Value(kind: vkBool, boolValue: x.boolValue) 
    of vkReference: result = Value(kind: vkReference, referenceValue: x.referenceValue)
    of vkEffect: result = Value(kind: vkEffect, effectValue: x.effectValue)
    of boxedValueKinds: discard # unreachable
  else:
    result = Value(kind: if unboxedKind: vkBoxed else: x.kind)
    result.boxedValue = toRef x

proc unboxStripType*(x: FullValueObj | FullValue): Value =
  case x.kind
  of vkNone: result = Value(kind: vkNone)
  of vkInt32: result = Value(kind: vkInt32, int32Value: x.int32Value)
  of vkUint32: result = Value(kind: vkUint32, uint32Value: x.uint32Value) 
  of vkFloat32: result = Value(kind: vkFloat32, float32Value: x.float32Value) 
  of vkBool: result = Value(kind: vkBool, boolValue: x.boolValue) 
  of vkEffect: result = Value(kind: vkEffect, effectValue: x.effectValue)
  of boxedValueKinds:
    result = Value(kind: x.kind)
    result.boxedValue = toRef x

proc getType*(x: Value): Type

proc getType*(x: FullValueObj): Type =
  if not x.type.isNil: return x.type[]
  case x.kind
  of vkNone: result = NoneValueTy
  of vkInt32: result = Int32Ty
  of vkUint32: result = Uint32Ty
  of vkFloat32: result = Float32Ty
  of vkInt64: result = Int64Ty
  of vkUint64: result = Uint64Ty
  of vkFloat64: result = Float64Ty
  of vkBool: result = BoolTy
  of vkReference: result = ReferenceTy[x.referenceValue[].getType]
  of vkBoxed: result = getType(x.boxedValue[])
  of vkList: result = ListTy[x.listValue.unref[0].getType]
  of vkString: result = StringTy
  of vkExpression: result = ExpressionTy
  of vkStatement: result = StatementTy
  of vkScope: result = ScopeTy
  of vkArray:
    let val = x.tupleValue.unref
    result = Type(kind: tyTuple, elements: newSeq[Type](val.len))
    for i in 0 ..< x.tupleValue.unref.len:
      result.elements[i] = val[i].getType
  of vkType:
    result = TypeTy[x.typeValue]
  of vkFunction, vkNativeFunction:
    result = Type(kind: tyBase, typeBase: FunctionTy)
    # could save signature into Function object
    # but we can still use the type field
  of vkSet:
    result = SetTy[AnyTy]
    for v in x.setValue:
      result.baseArguments[0] = v.getType
      break
  of vkTable:
    result = TableTy[AnyTy, AnyTy]
    for k, v in x.tableValue:
      result.baseArguments[0] = k.getType
      result.baseArguments[1] = v.getType
      break
  of vkEffect:
    # probably should never be here
    result = x.effectValue.unref.getType

proc getType*(x: Value): Type =
  case x.kind
  of vkNone: result = NoneValueTy
  of vkInt32: result = Int32Ty
  of vkUint32: result = Uint32Ty
  of vkFloat32: result = Float32Ty
  of vkInt64: result = Int64Ty
  of vkUint64: result = Uint64Ty
  of vkFloat64: result = Float64Ty
  of vkBool: result = BoolTy
  of vkReference: result = ReferenceTy[x.referenceValue[].getType]
  of boxedValueKinds - {vkInt64, vkUint64, vkFloat64}:
    result = x.boxedValue[].getType
  of vkEffect:
    # probably should never be here
    result = x.effectValue.unref.getType

when false:
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
    of vkEffect,
      vkSet, vkTable, vkExpression, vkStatement, vkScope:
      # unimplemented
      value

  import ./pointertag

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
