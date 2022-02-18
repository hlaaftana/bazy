import ".."/[primitives, compilation, types, values], ../../language/expressions

import common

module syntax:
  templ "block", 2:
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
        argTypes[i] = scope.evaluateStatic(arg.right, +Type(kind: tyType, typeValue: toRef(Ty(Any)))).typeValue[]
        arg = arg.left
      else:
        argTypes[i] = Ty(Any)
      discard bodyScope.define($arg, argTypes[i])
    var fnType = Type(kind: tyFunction, returnType: toRef(returnBound.boundType), arguments: toRef(argTypes))
    var v: Variable
    if name.len != 0:
      v = scope.define(name, fnType)
    let body = bodyScope.compile(body, returnBound)
    if not v.isNil and not returnBoundSet:
      v.cachedType.returnType[] = body.cachedType
    bodyScope.context.refreshStack()
    let fun = toValue(
      Function(stack: bodyScope.context.stack.shallowRefresh(), instruction: body.toInstruction))
    if not v.isNil:
      context.refreshStack()
      scope.context.stack.set(v.stackIndex, fun)
    result = Statement(kind: skArmStack,
      cachedType: fnType,
      armStackFunction: constant(fun, fnType))
  templ "=>", 2:
    var lhs = args[0]
    var body = args[1]
    let (bound, typeSet) =
      if lhs.kind == Colon:
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: toRef(Ty(Any)))).typeValue[]
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
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: toRef(Ty(Any)))).typeValue[]
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
        let t = scope.evaluateStatic(lhs.right, +Type(kind: tyType, typeValue: toRef(Ty(Any)))).typeValue[]
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
  templ "if", 2:
    let sc = scope.childScope()
    result = toValue Statement(kind: skIf,
      ifCond: sc.compile(args[0], +Ty(Boolean)),
      ifTrue: sc.compile(args[1], +Ty(Any)),
      ifFalse: Statement(kind: skNone))
  templ "if", 3:
    var els = args[2]
    if els.kind == Colon and els.left.isIdentifier(ident) and ident == "else":
      els = els.right
    let sc = scope.childScope()
    let elsesc = scope.childScope()
    var res = Statement(kind: skIf,
      ifCond: sc.compile(args[0], +Ty(Boolean)),
      ifTrue: sc.compile(args[1], +Ty(Any)),
      ifFalse: elsesc.compile(els, +Ty(Any)))
    res.cachedType = commonType(res.ifTrue.cachedType, res.ifFalse.cachedType)
    result = toValue(res)
  templ "while", 2:
    let sc = scope.childScope()
    result = toValue Statement(kind: skWhile,
      whileCond: sc.compile(args[0], +Ty(Boolean)),
      whileBody: sc.compile(args[1], -Type(kind: tyUnion, operands: toRef(seq[Type] @[]))))
  when false: # this has to be a typed template
    fn ".[]", [Type(kind: tyBaseType, baseKind: tyTuple), Ty(Integer)], Ty(Any):
      args[0].tupleValue.unref[args[1].integerValue]
  # todo: let/for
