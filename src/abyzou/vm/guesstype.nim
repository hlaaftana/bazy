import
  std/[sets, tables],
  ../util/box,
  ./[primitives, typebasics, arrays]

proc getType*(x: Value): Type =
  template boxedType(y) =
    if not y.type.isNoType:
      return y.type
  case x.kind
  of vNone: result = NoneValueTy
  of vInt32: result = Int32Ty
  of vUint32: result = Uint32Ty
  of vFloat32: result = Float32Ty
  of vInt64:
    boxedType x.int64Value
    result = Int64Ty
  of vUint64:
    boxedType x.uint64Value
    result = Uint64Ty
  of vFloat64:
    boxedType x.float64Value
    result = Float64Ty
  of vBool: result = BoolTy
  of vReference: result = ReferenceTy[x.referenceValue.unref.getType]
  of vBoxed:
    boxedType x.boxedValue
    result = getType(x.boxedValue.value)
  of vList:
    boxedType x.listValue
    result = ListTy[x.listValue.value.unref[0].getType]
  of vString:
    boxedType x.stringValue
    result = StringTy
  of vExpression: result = ExpressionTy
  of vStatement: result = StatementTy
  of vScope: result = ScopeTy
  of vArray:
    let val = x.tupleValue
    var elems = newSeq[Type](val.len)
    for i in 0 ..< x.tupleValue.len:
      elems[i] = val[i].getType
    result = Type(kind: tyTuple, elements: elems)
  of vType:
    result = x.typeValue.type
  of vFunction, vLinearFunction, vNativeFunction:
    case x.kind
    of vFunction:
      boxedType x.functionValue
    of vLinearFunction:
      boxedType x.linearFunctionValue
    else: discard
    result = Type(kind: tyBase, typeBase: FunctionTy)
    # could save signature into Function object
    # but we can still use the type field in FullValue
  of vSet:
    boxedType x.setValue
    result = SetTy[AnyTy]
    for v in x.setValue.value:
      result.baseArguments[0] = v.getType
      break
  of vTable:
    boxedType x.tableValue
    result = TableTy[AnyTy, AnyTy]
    for k, v in x.tableValue.value:
      result.baseArguments[0] = k.getType
      result.baseArguments[1] = v.getType
      break
  of vEffect:
    # probably should never be here
    result = x.effectValue.unref.getType
