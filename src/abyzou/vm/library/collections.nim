import ".."/[primitives, compilation, values, types]

import common

module collections:
  block reference:
    # todo: better names (maybe just copy nim)
    let T = Type(kind: tyParameter, parameter: newTypeParameter("T"))
    let rf = define(result, "ref", funcType(Type(kind: tyReference, elementType: box T), [T]))
    rf.genericParams = @[T.parameter]
    result.top.define(rf)
    let urf = define(result, "unref", funcType(T, [Type(kind: tyReference, elementType: box T)]))
    urf.genericParams = @[T.parameter]
    result.top.define(urf)
    let upd = define(result, "update", funcType(Ty(None), [Type(kind: tyReference, elementType: box T), T]))
    upd.genericParams = @[T.parameter]
    result.top.define(upd)
    result.refreshStack()
    result.stack.set rf.stackIndex, toValue proc (args: openarray[Value]): Value =
      result = Value(kind: vkReference)
      new(result.referenceValue)
      result.referenceValue[] = args[0].toFullValueObj
    result.stack.set urf.stackIndex, toValue proc (args: openarray[Value]): Value =
      args[0].referenceValue[].toSmallValue
    result.stack.set upd.stackIndex, toValue proc (args: openarray[Value]): Value =
      args[0].referenceValue[] = args[1].toFullValueObj
  define ".[]", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Statement)]).withProperties(
    property(Meta, funcType(Ty(Any), [Type(kind: tyBaseType, baseKind: tyTuple), Ty(Int32)]))
  ), toValue proc (valueArgs: openarray[Value]): Value =
    let scope = valueArgs[0].boxedValue.scopeValue
    let index = scope.context.evaluateStatic(valueArgs[2].boxedValue.statementValue.toInstruction)
    let nthType = valueArgs[1].boxedValue.statementValue.knownType.nth(index.int32Value)
    result = toValue Statement(kind: skGetIndex,
      knownType: nthType,
      getIndexAddress: valueArgs[1].boxedValue.statementValue,
      getIndex: index.int32Value)
