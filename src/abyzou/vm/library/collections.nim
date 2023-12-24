import
  ../[primitives, compilation, valueconstr, typebasics, treewalk]

import common

module collections:
  block reference:
    # todo: better names (maybe just copy nim)
    let T = Type(kind: tyParameter, parameter: newTypeParameter("T"))
    let rf = define(result, "ref", funcType(ReferenceTy[T], [T]))
    rf.genericParams = @[T.parameter]
    result.define(rf)
    let urf = define(result, "unref", funcType(T, [ReferenceTy[T]]))
    urf.genericParams = @[T.parameter]
    result.define(urf)
    let upd = define(result, "update", funcType(NoneTy, [ReferenceTy[T], T]))
    upd.genericParams = @[T.parameter]
    result.define(upd)
    result.context.refreshStack()
    result.context.stack.set rf.stackIndex, toValue proc (args: openarray[Value]): Value =
      result = Value(kind: vkReference)
      new(result.referenceValue.realRef)
      result.referenceValue.realRef[] = args[0].toFullValueObj
    result.context.stack.set urf.stackIndex, toValue proc (args: openarray[Value]): Value =
      args[0].referenceValue.unref.toSmallValue
    result.context.stack.set upd.stackIndex, toValue proc (args: openarray[Value]): Value =
      args[0].referenceValue.realRef[] = args[1].toFullValueObj
  define ".[]", funcType(StatementTy, [ScopeTy, StatementTy, StatementTy]).withProperties(
    property(Meta, funcType(AnyTy, [Type(kind: tyBase, typeBase: TupleTy), Int32Ty]))
  ), toValue proc (valueArgs: openarray[Value]): Value =
    let scope = valueArgs[0].boxedValue.scopeValue
    let index = scope.context.evaluateStatic(valueArgs[2].boxedValue.statementValue.toInstruction)
    let nthType = valueArgs[1].boxedValue.statementValue.knownType.nth(index.int32Value)
    result = toValue Statement(kind: skGetIndex,
      knownType: nthType,
      getIndexAddress: valueArgs[1].boxedValue.statementValue,
      getIndex: index.int32Value)
