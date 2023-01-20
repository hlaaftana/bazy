import ".."/[primitives, compilation, values, types]

import common

module collections:
  define ".[]", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Statement)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Type(kind: tyBaseType, baseKind: tyTuple), Ty(Int32)]))
  ), toValue proc (valueArgs: openarray[Value]): Value =
    let scope = valueArgs[0].boxedValue.scopeValue
    let index = scope.context.evaluateStatic(valueArgs[2].boxedValue.statementValue.toInstruction)
    let nthType = valueArgs[1].boxedValue.statementValue.knownType.nth(index.int32Value)
    result = toValue Statement(kind: skGetIndex,
      knownType: nthType,
      getIndexAddress: valueArgs[1].boxedValue.statementValue,
      getIndex: index.int32Value)
