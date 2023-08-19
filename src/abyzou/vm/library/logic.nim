import ".."/[primitives, compilation, values, types], ../../language/[expressions, shortstring]

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
      property(Meta, toValue funcTypeWithVarargs(Ty(Bool), [], Ty(Bool)))),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: res,
            ifFalse: constant(false, Ty(Bool)),
            knownType: Ty(Bool))
      result = toValue res
  define "or",
    funcTypeWithVarargs(Ty(Statement), [Ty(Scope)], Ty(Statement)).withProperties(
      property(Meta, toValue funcTypeWithVarargs(Ty(Bool), [], Ty(Bool)))),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: constant(true, Ty(Bool)),
            ifFalse: res,
            knownType: Ty(Bool))
      result = toValue res
  fn "xor", [Ty(Bool), Ty(Bool)], Ty(Bool):
    toValue(args[0].boolValue xor args[1].boolValue)
  define "if", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Ty(Bool), Ty(Any)]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].boxedValue.scopeValue.childScope()
    result = toValue Statement(kind: skIf,
      ifCond: valueArgs[1].boxedValue.statementValue,
      ifTrue: sc.compile(valueArgs[2].boxedValue.expressionValue, +Ty(Any)),
      ifFalse: Statement(kind: skNone))
  define "if", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Ty(Bool), Ty(Any), Ty(Any)]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    var els = valueArgs[3].boxedValue.expressionValue
    if els.kind == Colon and els.left.isIdentifier(ident) and ident == "else":
      els = els.right
    let scope = valueArgs[0].boxedValue.scopeValue
    let sc = scope.childScope()
    let elsesc = scope.childScope()
    var res = Statement(kind: skIf,
      ifCond: valueArgs[1].boxedValue.statementValue,
      ifTrue: sc.compile(valueArgs[2].boxedValue.expressionValue, +Ty(Any)),
      ifFalse: elsesc.compile(els, +Ty(Any)))
    res.knownType = commonSuperType(res.ifTrue.knownType, res.ifFalse.knownType)
    result = toValue(res)
  define "while", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(union(), [Ty(Bool), union()]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].boxedValue.scopeValue.childScope()
    result = toValue Statement(kind: skWhile,
      whileCond: valueArgs[1].boxedValue.statementValue,
      whileBody: sc.compile(valueArgs[2].boxedValue.expressionValue, -union()))
