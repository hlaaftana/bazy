import
  std/[sets, tables],
  ../util/box,
  ./[primitives, typebasics, arrays]

proc getType*(x: Value): Type =
  template boxedType(y) =
    if not y.type.isNoType:
      return y.type
  case x.kind
  of vkNone: result = NoneValueTy
  of vkInt32: result = Int32Ty
  of vkUint32: result = Uint32Ty
  of vkFloat32: result = Float32Ty
  of vkInt64:
    boxedType x.int64Value
    result = Int64Ty
  of vkUint64:
    boxedType x.uint64Value
    result = Uint64Ty
  of vkFloat64:
    boxedType x.float64Value
    result = Float64Ty
  of vkBool: result = BoolTy
  of vkReference: result = ReferenceTy[x.referenceValue.unref.getType]
  of vkBoxed:
    boxedType x.boxedValue
    result = getType(x.boxedValue.value)
  of vkList:
    boxedType x.listValue
    result = ListTy[x.listValue.value.unref[0].getType]
  of vkString:
    boxedType x.stringValue
    result = StringTy
  of vkExpression: result = ExpressionTy
  of vkStatement: result = StatementTy
  of vkScope: result = ScopeTy
  of vkArray:
    let val = x.tupleValue.unref
    var elems = newSeq[Type](val.len)
    for i in 0 ..< x.tupleValue.unref.len:
      elems[i] = val[i].getType
    result = Type(kind: tyTuple, elements: elems)
  of vkType:
    result = x.typeValue.type
  of vkFunction, vkLinearFunction, vkNativeFunction:
    case x.kind
    of vkFunction:
      boxedType x.functionValue
    of vkLinearFunction:
      boxedType x.linearFunctionValue
    else: discard
    result = Type(kind: tyBase, typeBase: FunctionTy)
    # could save signature into Function object
    # but we can still use the type field in FullValue
  of vkSet:
    boxedType x.setValue
    result = SetTy[AnyTy]
    for v in x.setValue.value:
      result.baseArguments[0] = v.getType
      break
  of vkTable:
    boxedType x.tableValue
    result = TableTy[AnyTy, AnyTy]
    for k, v in x.tableValue.value:
      result.baseArguments[0] = k.getType
      result.baseArguments[1] = v.getType
      break
  of vkEffect:
    # probably should never be here
    result = x.effectValue.unref.getType
