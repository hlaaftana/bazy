import ".."/[primitives, compilation, values, types]

import common

module logic:
  define "Boolean", Ty(Boolean)
  define "true", true
  define "false", false
  fn "not", [Ty(Boolean)], Ty(Boolean):
    toValue not bool(args[0].integerValue)
  define "and",
    funcTypeWithVarargs(Ty(Statement), [Ty(Scope)], Ty(Statement)).withProperties(
      property(Meta, @[toValue funcTypeWithVarargs(Ty(Boolean), [], Ty(Boolean))])),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].statementValue,
            ifTrue: res,
            ifFalse: constant(false, Ty(Boolean)),
            cachedType: Ty(Boolean))
      result = toValue res
  define "or",
    funcTypeWithVarargs(Ty(Statement), [Ty(Scope)], Ty(Statement)).withProperties(
      property(Meta, @[toValue funcTypeWithVarargs(Ty(Boolean), [], Ty(Boolean))])),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].statementValue,
            ifTrue: constant(true, Ty(Boolean)),
            ifFalse: res,
            cachedType: Ty(Boolean))
      result = toValue res
