import ".."/[primitives, compilation, types, values, arrays], ../../language/expressions

import common

module syntax:
  templ "block", 1:
    let sc = scope.childScope()
    result = toValue sc.compile(args[0], +Ty(Any))
  templ "static", 1:
    let st = scope.compile(args[0], +Ty(Any))
    result = toValue constant(scope.context.evaluateStatic(st.toInstruction), st.cachedType)
  
  proc makeFn(scope: Scope, arguments: seq[Expression], body: Expression,
    name: string, returnBound: TypeBound, returnBoundSet: bool): Statement =
    let context = scope.context.childContext()
    let bodyScope = context.top
    var argTypes = newSeq[Type](arguments.len)
    for i in 0 ..< arguments.len:
      var arg = arguments[i]
      if arg.kind == Colon:
        argTypes[i] = scope.evaluateStatic(arg.right, +Type(kind: tyType, typeValue: box Ty(Any))).typeValue.unbox
        arg = arg.left
      else:
        argTypes[i] = Ty(Any)
      discard bodyScope.define($arg, argTypes[i])
    var fnType = funcType(returnBound.boundType, argTypes)
    var v: Variable
    if name.len != 0:
      v = scope.define(name, fnType)
    let body = bodyScope.compile(body, returnBound)
    if not v.isNil and not returnBoundSet:
      v.cachedType.returnType = body.cachedType.box
    bodyScope.context.refreshStack()
    let fun = toValue(
      Function(stack: bodyScope.context.stack.shallowRefresh(), instruction: body.toInstruction))
    if not v.isNil:
      context.refreshStack()
      scope.context.stack.set(v.stackIndex, fun)
    result = Statement(kind: skArmStack,
      cachedType: fnType,
      armStackFunction: constant(fun, fnType))

  type
    AssignBehavior = enum asDefine, asSet

    AssignError = object of CompileError
      varName: string
      scope: Scope

  proc assign(scope: Scope, bound: TypeBound, full: Expression, lhs, rhs: Expression, behaviors: set[AssignBehavior]): Statement =
    # XXX (0) generics
    var lhs = lhs
    let typeExpr =
      if lhs.kind == Colon:
        #let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: Ty(Any).box)).typeValue.unbox
        let val = lhs.right
        lhs = lhs.left
        val
      else: nil
    let arguments =
      case lhs.kind
      of CallKinds:
        (lhs.arguments, true)
      of Wrapped:
        (@[lhs.wrapped], true)
      else:
        (@[], false)
    let genericArguments =
      if lhs.kind == Subscript:
        let val = lhs.arguments
        lhs = lhs.address
        (val, true)
      else:
        (@[], false)
    # XXX (0) finish
    case lhs.kind
    of Name, Symbol:
      let name = $lhs
      if asSet in behaviors and (let a = scope.overloads(name, bound); a.len != 0):
        let v = a[0]
        let value = compile(scope, rhs, +v.variable.cachedType)
        result = variableSet(v, value)
      elif asDefine in behaviors:
        let value = compile(scope, rhs, bound)
        let v = scope.define(name, value.cachedType)
        result = variableSet(v.shallowReference, value)
      else:
        raise (ref AssignError)(
          expression: full,
          varName: name,
          scope: scope,
          msg: "cannot assign to " & name)
    of CallKinds:
      result = makeFn(scope, lhs.arguments, rhs, $lhs.address, bound, typeExpr != nil)
    else: assert false, $lhs
  
  templ "=>", 2:
    var lhs = args[0]
    var body = args[1]
    let (bound, typeSet) =
      if lhs.kind == Colon:
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: Ty(Any).box)).typeValue.unbox
        lhs = lhs.left
        (+t, true)
      else:
        (+Ty(Any), false)
    let (name, arguments) =
      if lhs.kind in CallKinds:
        ($lhs.address, lhs.arguments)
      elif lhs.kind == Wrapped:
        ("", @[lhs.wrapped])
      else:
        ("", lhs.elements)
    result = toValue makeFn(scope, arguments, body, name, bound, typeSet)
  templ ":=", 2:
    var lhs = args[0]
    let rhs = args[1]
    let (bound, typeSet) =
      if lhs.kind == Colon:
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: Ty(Any).box)).typeValue.unbox
        lhs = lhs.left
        (+t, true)
      else:
        (+Ty(Any), false)
    case lhs.kind
    of Name, Symbol:
      let name = $lhs
      let value = compile(scope, rhs, bound)
      let v = scope.define(name, if typeSet: bound.boundType else: value.cachedType)
      result = toValue variableSet(v.shallowReference, value)
    of CallKinds:
      result = toValue makeFn(scope, lhs.arguments, rhs, $lhs.address, bound, typeSet)
    else: assert false, $lhs
  templ "=", 2:
    var lhs = args[0]
    let rhs = args[1]
    let (bound, typeSet) =
      if lhs.kind == Colon:
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: Ty(Any).box)).typeValue.unbox
        lhs = lhs.left
        (+t, true)
      else:
        (+Ty(Any), false)
    case lhs.kind
    of Name, Symbol:
      let name = $lhs
      if (let a = scope.overloads(name, bound); a.len != 0):
        let v = a[0]
        let value = compile(scope, rhs, +v.variable.cachedType)
        result = toValue variableSet(v, value)
      else:
        let value = compile(scope, rhs, bound)
        let v = scope.define(name, value.cachedType)
        result = toValue variableSet(v.shallowReference, value)
    of CallKinds:
      result = toValue makeFn(scope, lhs.arguments, rhs, $lhs.address, bound, typeSet)
    else: assert false, $lhs
  define "if", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Ty(Boolean), Ty(Any)]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].scopeValue.childScope()
    result = toValue Statement(kind: skIf,
      ifCond: valueArgs[1].statementValue,
      ifTrue: sc.compile(valueArgs[2].expressionValue, +Ty(Any)),
      ifFalse: Statement(kind: skNone))
  define "if", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Ty(Boolean), Ty(Any), Ty(Any)]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    var els = valueArgs[3].expressionValue
    if els.kind == Colon and els.left.isIdentifier(ident) and ident == "else":
      els = els.right
    let scope = valueArgs[0].scopeValue
    let sc = scope.childScope()
    let elsesc = scope.childScope()
    var res = Statement(kind: skIf,
      ifCond: valueArgs[1].statementValue,
      ifTrue: sc.compile(valueArgs[2].expressionValue, +Ty(Any)),
      ifFalse: elsesc.compile(els, +Ty(Any)))
    res.cachedType = commonSuperType(res.ifTrue.cachedType, res.ifFalse.cachedType)
    result = toValue(res)
  define "while", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Expression)]).withProperties(
    property(Meta, toValue funcType(union(), [Ty(Boolean), union()]))
  ), toValue proc (valueArgs: openarray[Value]): Value = 
    let sc = valueArgs[0].scopeValue.childScope()
    result = toValue Statement(kind: skWhile,
      whileCond: valueArgs[1].statementValue,
      whileBody: sc.compile(valueArgs[2].expressionValue, -union()))
  define ".[]", funcType(Ty(Statement), [Ty(Scope), Ty(Statement), Ty(Statement)]).withProperties(
    property(Meta, toValue funcType(Ty(Any), [Type(kind: tyBaseType, baseKind: tyTuple), Ty(Integer)]))
  ), toValue proc (valueArgs: openarray[Value]): Value =
    let scope = valueArgs[0].scopeValue
    let index = scope.context.evaluateStatic(valueArgs[2].statementValue.toInstruction)
    let nthType = valueArgs[1].statementValue.cachedType.nth(index.integerValue)
    proc val(args: openarray[Value]): Value {.nimcall.} =
      args[0].tupleValue.unref[args[1].integerValue]
    result = toValue Statement(kind: skFunctionCall,
      cachedType: nthType,
      callee: constant(
        Value(kind: vkNativeFunction, nativeFunctionValue: val),
        tyNone),
      arguments: @[valueArgs[1].statementValue, constant(index, Ty(Integer))])
  # todo: let/for
