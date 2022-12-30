import ".."/[primitives, compilation, values, types]

import common

module logic:
  define "Boolean", Ty(Bool)
  define "true", true
  define "false", false
  fn "not", [Ty(Bool)], Ty(Bool):
    toValue not args[0].boolValue
  # these don't need to be varargs, they just are to make sure varargs work
  define "and",
    funcTypeWithVarargs(Ty(Statement), [Ty(Scope)], Ty(Statement)).withProperties(
      property(Meta, @[toValue funcTypeWithVarargs(Ty(Bool), [], Ty(Bool))])),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: res,
            ifFalse: constant(false, Ty(Bool)),
            cachedType: Ty(Bool))
      result = toValue res
  define "or",
    funcTypeWithVarargs(Ty(Statement), [Ty(Scope)], Ty(Statement)).withProperties(
      property(Meta, @[toValue funcTypeWithVarargs(Ty(Bool), [], Ty(Bool))])),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: constant(true, Ty(Bool)),
            ifFalse: res,
            cachedType: Ty(Bool))
      result = toValue res
  fn "xor", [Ty(Bool), Ty(Bool)], Ty(Bool):
    toValue(args[0].boolValue xor args[1].boolValue)
