import ".."/[primitives, compilation, values, types], ../../language/[expressions, shortstring]

import common

module logic:
  define "Boolean", BoolTy
  define "true", true
  define "false", false
  fn "not", [BoolTy], BoolTy:
    toValue not args[0].boolValue
  # these don't need to be varargs, they just are to make sure varargs work
  define "and",
    funcTypeWithVarargs(StatementTy, [ScopeTy], StatementTy).withProperties(
      property(Meta, funcTypeWithVarargs(BoolTy, [], BoolTy))),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: res,
            ifFalse: constant(false, BoolTy),
            knownType: BoolTy)
      result = toValue res
  define "or",
    funcTypeWithVarargs(StatementTy, [ScopeTy], StatementTy).withProperties(
      property(Meta, funcTypeWithVarargs(BoolTy, [], BoolTy))),
    toValue proc (valueArgs: openarray[Value]): Value =
      var res = valueArgs[^1].boxedValue.statementValue
      if valueArgs.len > 1:
        for i in countdown(valueArgs.len - 2, 1):
          res = Statement(kind: skIf,
            ifCond: valueArgs[i].boxedValue.statementValue,
            ifTrue: constant(true, BoolTy),
            ifFalse: res,
            knownType: BoolTy)
      result = toValue res
  fn "xor", [BoolTy, BoolTy], BoolTy:
    toValue(args[0].boolValue xor args[1].boolValue)
  define "if", funcType(StatementTy, [ScopeTy, StatementTy, ExpressionTy]).withProperties(
    property(Meta, funcType(AnyTy, [BoolTy, AnyTy]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].boxedValue.scopeValue.childScope()
    result = toValue Statement(kind: skIf,
      ifCond: valueArgs[1].boxedValue.statementValue,
      ifTrue: sc.compile(valueArgs[2].boxedValue.expressionValue, +AnyTy),
      ifFalse: Statement(kind: skNone))
  define "if", funcType(StatementTy, [ScopeTy, StatementTy, ExpressionTy, ExpressionTy]).withProperties(
    property(Meta, funcType(AnyTy, [BoolTy, AnyTy, AnyTy]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    var els = valueArgs[3].boxedValue.expressionValue
    if els.kind == Colon and els.left.isIdentifier(ident) and ident == "else":
      els = els.right
    let scope = valueArgs[0].boxedValue.scopeValue
    let sc = scope.childScope()
    let elsesc = scope.childScope()
    var res = Statement(kind: skIf,
      ifCond: valueArgs[1].boxedValue.statementValue,
      ifTrue: sc.compile(valueArgs[2].boxedValue.expressionValue, +AnyTy),
      ifFalse: elsesc.compile(els, +AnyTy))
    res.knownType = commonSuperType(res.ifTrue.knownType, res.ifFalse.knownType)
    result = toValue(res)
  define "while", funcType(StatementTy, [ScopeTy, StatementTy, ExpressionTy]).withProperties(
    property(Meta, funcType(union(), [BoolTy, union()]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].boxedValue.scopeValue.childScope()
    result = toValue Statement(kind: skWhile,
      whileCond: valueArgs[1].boxedValue.statementValue,
      whileBody: sc.compile(valueArgs[2].boxedValue.expressionValue, -union()))
