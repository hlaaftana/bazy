import ".."/[primitives, compilation, types, values], ../../language/expressions

import common

module syntax:
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
      let name = lhs.identifier
      let value = compile(scope, rhs, bound)
      let v = scope.define(name, if typeSet: bound.boundType else: value.cachedType)
      result = toValue variableSet(v.shallowReference, value)
    of CallKinds:
      let name = lhs.address.identifier
      let context = scope.context.childContext()
      let bodyScope = context.top
      var argTypes = newSeq[Type](lhs.arguments.len)
      for i in 0 ..< lhs.arguments.len:
        var arg = lhs.arguments[i]
        if arg.kind == Colon:
          argTypes[i] = scope.evaluateStatic(arg.right, +Type(kind: tyType, typeValue: toRef(Ty(Any)))).typeValue[]
          arg = arg.left
        else:
          argTypes[i] = Ty(Any)
        discard bodyScope.define(arg.identifier, argTypes[i])
      var fnType = Type(kind: tyFunction, returnType: toRef(bound.boundType), arguments: toRef(argTypes))
      var v = scope.define(name, fnType)
      let body = bodyScope.compile(rhs, bound)
      if not typeSet:
        v.cachedType.returnType[] = body.cachedType
      context.refreshStack()
      scope.context.stack.set(v.stackIndex, toValue(
        Function(stack: bodyScope.context.stack.shallowRefresh(), instruction: body.toInstruction)))
      result = toValue Statement(kind: skNone)
    else: assert false, $lhs
  templ "if", 2:
    let sc = scope.childScope()
    result = toValue Statement(kind: skIf,
      ifCond: sc.compile(args[0], +Ty(Boolean)),
      ifTrue: sc.compile(args[1], +Ty(Any)),
      ifFalse: Statement(kind: skNone))
  templ "if", 3:
    var els = args[2]
    if els.kind == Colon and els.left.kind in {Name, Symbol} and els.left.identifier == "else":
      els = els.right
    let sc = scope.childScope()
    let elsesc = scope.childScope()
    var res = Statement(kind: skIf,
      ifCond: sc.compile(args[0], +Ty(Boolean)),
      ifTrue: sc.compile(args[1], +Ty(Any)),
      ifFalse: elsesc.compile(els, +Ty(Any)))
    res.cachedType = commonType(res.ifTrue.cachedType, res.ifFalse.cachedType)
    result = toValue(res)
