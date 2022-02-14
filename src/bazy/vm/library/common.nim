import ".."/[primitives, compilation, types, values], ../../language/expressions, std/[sets]

proc define*(scope: Scope, n: string, typ: Type): Variable =
  result = Variable(name: n, cachedType: typ)
  scope.define(result)

proc define*(scope: Scope, n: string, typ: Type, x: Value) =
  let variable = define(scope, n, typ)
  scope.context.refreshStack()
  scope.context.stack.set(variable.stackIndex, x)

proc define*(context: Context, n: string, typ: Type): Variable {.inline.} =
  context.top.define(n, typ)

proc define*(context: Context, n: string, typ: Type, x: Value) =
  let variable = define(context, n, typ)
  context.refreshStack()
  context.stack.set(variable.stackIndex, x)

proc funcType*(returnType: Type, arguments: varargs[Type]): Type {.inline.} =
  Type(kind: tyFunction, returnType: toRef(returnType), arguments: toRef(@arguments))

proc templType*(arity: int): Type {.inline.} =
  var args = newSeq[Type](arity + 1)
  args[0] = Ty(Scope)
  for i in 0 ..< arity:
    args[i + 1] = Ty(Expression)
  result = funcType(Ty(Statement), args)
  result.properties.incl(toValue(Template))

template templ*(body): untyped =
  (proc (valueArgs: openarray[Value]): Value {.nimcall.} =
    let scope {.inject.} = valueArgs[0].scopeValue
    var args {.inject.} = newSeq[Expression](valueArgs.len - 1)
    for i in 0 ..< args.len:
      args[i] = valueArgs[i + 1].expressionValue
    body)

proc typedTemplType*(arity: int): Type {.inline.} =
  var args = newSeq[Type](arity + 1)
  args[0] = Ty(Scope)
  for i in 0 ..< arity:
    args[i + 1] = Ty(Statement)
  result = funcType(Ty(Statement), args)
  result.properties.incl(toValue(TypedTemplate))

template typedTempl*(body): untyped =
  (proc (valueArgs: openarray[Value]): Value {.nimcall.} =
    let scope {.inject.} = valueArgs[0].scopeValue
    var args {.inject.} = newSeq[Statement](valueArgs.len - 1)
    for i in 0 ..< args.len:
      args[i] = valueArgs[i + 1].statementValue
    body)

template fn*(body): untyped =
  (proc (args {.inject.}: openarray[Value]): Value {.nimcall.} =
    body)

template module*(moduleName, definitions): untyped {.dirty.} =
  proc `moduleName`*: Context =
    result = newContext(@[])
    template define(n: string, typ: Type, x: Value) {.used.} =
      define(result, n, typ, x)
    template define(n: string, x: Value) {.used.} =
      let value = x
      define(n, toType(value), value)
    template define(n: string, typ: Type, x) {.used.} =
      define(n, typ, toValue(x))
    template define(n: string, x) {.used.} =
      define(n, toValue(x))
    template templ(n: string, arity: int, body: untyped) {.used.} =
      define n, templType(arity), templ(body)
    template typedTempl(n: string, arity: int, body: untyped) {.used.} =
      define n, typedTemplType(arity), typedTempl(body)
    template fn(n: string, arguments: openarray[Type], returnType: Type, body: untyped) {.used.} =
      define n, funcType(returnType, arguments), fn(body)
    definitions
