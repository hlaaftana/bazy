import
  std/[sets, tables],
  ../util/box,
  ./[primitives, typebasics, arrays]

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
  of vkReference: result = ReferenceTy[x.referenceValue.unref.getType]
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
    # but we can still use the type field in FullValue
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
  of vkReference: result = ReferenceTy[x.referenceValue.unref.getType]
  of boxedValueKinds - {vkInt64, vkUint64, vkFloat64}:
    result = x.boxedValue[].getType
  of vkEffect:
    # probably should never be here
    result = x.effectValue.unref.getType
