import "."/[primitives, arrays, runtime, types, values], ../language/[expressions, number], std/[tables, sets, strutils]

proc newContext*(imports: seq[Context]): Context =
  result = Context(stack: Stack(), imports: imports)
  result.top = Scope(context: result)

proc refreshStack*(context: Context) =
  ## grow static stack if new variables have been added
  ## recursively do the same for imports
  ## should be called after full compilation
  for im in context.imports:
    im.refreshStack()
  if context.imports.len != context.stack.imports.len:
    var newImports = newArray[Stack](context.imports.len)
    for i in 0 ..< context.stack.imports.len:
      newImports[i] = context.stack.imports[i]
    for i in context.stack.imports.len ..< context.imports.len:
      newImports[i] = context.imports[i].stack
    context.stack.imports = newImports
  if context.allVariables.len != context.stack.stack.len:
    var newStack = newArray[Value](context.allVariables.len)
    for i in 0 ..< context.stack.stack.len:
      newStack[i] = context.stack.stack[i]
    context.stack.stack = newStack

proc evaluateStatic*(context: Context, instr: Instruction): Value =
  context.refreshStack()
  instr.evaluate(context.stack)

proc define*(scope: Scope, variable: Variable) =
  variable.scope = scope
  variable.stackIndex = scope.context.allVariables.len
  scope.context.allVariables.add(variable)
  scope.variables.add(variable)

proc toInstruction*(st: Statement): Instruction =
  template map(s: Statement): Instruction =
    s.toInstruction
  template map(s: (Statement, Statement)): (Instruction, Instruction) =
    (s[0].toInstruction, s[1].toInstruction)
  template map(s: seq): Array =
    var arr = newArray[typeof map s[0]](s.len)
    for i in 0 ..< arr.len:
      arr[i] = map s[i]
    arr
  result = case st.kind
  of skNone: Instruction(kind: NoOp)
  of skConstant: Instruction(kind: Constant, constantValue: st.constant)
  of skFunctionCall:
    Instruction(kind: FunctionCall, function: map st.callee,
      arguments: map st.arguments)
  of skSequence: Instruction(kind: Sequence, sequence: map st.sequence)
  of skVariableGet:
    Instruction(kind: VariableGet, variableGetIndex: st.variableGetIndex)
  of skVariableSet:
    Instruction(kind: VariableSet, variableSetIndex: st.variableSetIndex,
      variableSetValue: map st.variableSetValue)
  of skFromImportedStack:
    Instruction(kind: FromImportedStack,
      importedStackIndex: st.importedStackIndex,
      importedStackInstruction: map st.importedStackStatement)
  of skIf:
    Instruction(kind: If, ifCondition: map st.ifCond,
      ifTrue: map st.ifTrue, ifFalse: map st.ifFalse)
  of skWhile:
    Instruction(kind: While, whileCondition: map st.whileCond,
      whileTrue: map st.whileBody)
  of skDoUntil:
    Instruction(kind: DoUntil, doUntilCondition: map st.doUntilCond,
      doUntilTrue: map st.doUntilBody)
  of skEmitEffect:
    Instruction(kind: EmitEffect, effect: map st.effect)
  of skHandleEffect:
    Instruction(kind: HandleEffect, effectHandler: map st.effectHandler,
      effectEmitter: map st.effectBody)
  of skTuple:
    Instruction(kind: BuildTuple, elements: map st.elements)
  of skList:
    Instruction(kind: BuildList, elements: map st.elements)
  of skSet:
    Instruction(kind: BuildSet, elements: map st.elements)
  of skTable:
    Instruction(kind: BuildTable, entries: map st.entries)

type
  CompileError* = object of CatchableError
    expression*: Expression

  TypeBoundMatchError* = object of CompileError
    bound*: TypeBound
    `type`*: Type
  
  NoOverloadFoundError* = object of CompileError
    bound*: TypeBound
    scope*: Scope

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement

proc setStatic*(variable: Variable, expression: Expression) =
  variable.scope.context.refreshStack()
  let value = variable.scope.compile(expression, +variable.cachedType)
  variable.cachedType = value.cachedType
  variable.scope.context.stack.set(variable.stackIndex, value.toInstruction.evaluate(variable.scope.context.stack))
  variable.evaluated = true

proc getType*(variable: Variable): Type =
  if not variable.lazyExpression.isNil and not variable.evaluated:
    variable.setStatic(variable.lazyExpression)
  variable.cachedType

proc shallowReference*(v: Variable): VariableReference {.inline.} =
  VariableReference(variable: v, address: VariableAddress(indices: @[v.stackIndex]))

proc symbols*(scope: Scope, name: string, bound: TypeBound, doImports = true): seq[VariableReference] =
  if scope.isNil: return
  if doImports:
    for i, im in scope.context.imports:
      let addrs = symbols(im.top, name, bound)
      for a in addrs:
        var b = a
        b.address.indices.add(i)
        result.add(b)
  if not scope.parent.isNil:
    result.add(symbols(scope.parent, name, bound, doImports = false))
  for i, v in scope.variables:
    if name == v.name and bound.matchBound(v.getType()):
      result.add(v.shallowReference)

import algorithm

proc overloads*(scope: Scope | Context, name: string, bound: TypeBound): seq[VariableReference] =
  result = symbols(scope, name, bound)
  # sort must be stable
  result.sort(
    cmp = proc (a, b: VariableReference): int =
      compare(a.variable.cachedType, b.variable.cachedType),
    order = if bound.variance.matches: Ascending else: Descending)
  result.reverse()

proc variableGet*(r: VariableReference): Statement =
  let t = r.variable.cachedType
  result = Statement(kind: skVariableGet,
    variableGetIndex: r.address.indices[0],
    cachedType: t)
  for i in 1 ..< r.address.indices.len:
    result = Statement(kind: skFromImportedStack,
      importedStackIndex: r.address.indices[i],
      importedStackStatement: result,
      cachedType: t)

proc variableSet*(r: VariableReference, value: Statement): Statement =
  let t = r.variable.cachedType
  result = Statement(kind: skVariableSet,
    variableSetIndex: r.address.indices[0],
    variableSetValue: value,
    cachedType: t)
  for i in 1 ..< r.address.indices.len:
    result = Statement(kind: skFromImportedStack,
      importedStackIndex: r.address.indices[i],
      importedStackStatement: result,
      cachedType: t)

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  template defaultBound: untyped = +makeType(Any)
  template map(ex: Expression, bound = defaultBound): Statement =
    compile(scope, ex, bound)
  template forward(ex: Expression): Statement =
    compile(scope, ex, bound)
  template constant(value: Value, ty: Type): Statement =
    Statement(kind: skConstant, constant: value, cachedType: ty)
  template constant(value: untyped, ty: Type): Statement =
    constant(toValue(value), ty)
  template constant(value: untyped, ty: TypeKind): Statement =
    constant(value, Type(kind: ty))
  template constant(value: string): Statement = constant(value, tyString)
  template constant(value: int): Statement = constant(value, tyInteger)
  template constant(value: uint): Statement = constant(value, tyUnsigned)
  template constant(value: float): Statement = constant(value, tyFloat)
  case ex.kind
  of None: result = Statement(kind: skNone, cachedType: makeType(None))
  of Number:
    let s = $ex.number
    result = Statement(kind: skConstant)
    case ex.number.kind
    of Integer:
      let val = parseInt(s)
      if bound.boundType.kind == tyFloat:
        result = constant(val.float)
      elif val >= 0 and bound.boundType.kind == tyUnsigned:
        result = constant(val.uint)
      else:
        result = constant(val)
    of Floating:
      result = constant(parseFloat(s))
    of Unsigned:
      result = constant(parseUInt(s))
  of String:
    result = constant(ex.str)
  of Wrapped:
    result = forward(ex.wrapped)
  of Name, Symbol:
    let overloads = overloads(scope, ex.identifier, bound)
    if overloads.len == 0:
      raise (ref NoOverloadFoundError)(
        expression: ex,
        bound: bound,
        msg: "no overloads with bound " & $bound & " for " & $ex)
    # XXX warn on ambiguity, thankfully recency is accounted for
    result = variableGet(overloads[0])
  of Dot:
    if ex.right.kind == Name:
      let lhs = map ex.left
      let name = ex.right.identifier
      if lhs.cachedType.kind == tyComposite and lhs.cachedType.fields.hasKey(name):
        result = Statement(kind: skFunctionCall,
          cachedType: lhs.cachedType.fields[name],
          callee: constant(
            Value(kind: vkNativeFunction, nativeFunctionValue: proc (args: openarray[Value]): Value {.nimcall.} =
              args[0].compositeValue[
                when args[1].stringValue is ref:
                  args[1].stringValue[]
                else:
                  $args[1].stringValue]),
            tyNone),
          arguments: @[lhs, constant(name)])
      else:
        let ident = Expression(kind: Symbol, identifier: "." & name)
        try:
          result = forward(
            Expression(kind: PathCall,
              address: ident,
              arguments: @[ex.left]))
        except NoOverloadFoundError as e:
          if not same(ident, e.expression):
            raise
    if result.isNil:
      result = forward(
        Expression(kind: PathCall,
          address: Expression(kind: Symbol, identifier: "."),
          arguments: @[ex.left, ex.right]))
  of OpenCall, Infix, Prefix, Postfix, PathCall, PathInfix, PathPrefix, PathPostfix:
    # XXX better templates
    if ex.address.kind in {Name, Symbol}:
      var argTypes = toRef(newSeq[Type](ex.arguments.len + 1))
      argTypes[][0] = makeType(Scope)
      for i in 0 ..< ex.arguments.len:
        argTypes[][i + 1] = makeType(Expression)
      var templateType = Type(kind: tyFunction,
        returnType: toRef(makeType(Statement)),
        arguments: argTypes)
      templateType = Type(kind: tyWithProperty, typeWithProperty: toRef(templateType), withProperty: toValue(Template))
      let overloads = overloads(scope, ex.address.identifier, +templateType)
      if overloads.len != 0:
        let templ = overloads[0]
        var arguments = newSeq[Statement](ex.arguments.len + 1)
        arguments[0] = constant(scope, makeType(Scope))
        for i in 0 ..< ex.arguments.len:
          arguments[i + 1] = constant(copy ex.arguments[i], makeType(Expression))
        let call = Statement(kind: skFunctionCall,
          callee: variableGet(templ),
          arguments: arguments).toInstruction
        result = scope.context.evaluateStatic(call).statementValue
    if result.isNil and ex.address.kind in {Name, Symbol}:
      var argTypes = toRef(newSeq[Type](ex.arguments.len + 1))
      argTypes[][0] = makeType(Scope)
      for i in 0 ..< ex.arguments.len:
        argTypes[][i + 1] = makeType(Statement)
      var typedTemplateType = Type(kind: tyFunction,
        returnType: toRef(makeType(Statement)),
        arguments: argTypes)
      typedTemplateType = Type(kind: tyWithProperty, typeWithProperty: toRef(typedTemplateType), withProperty: toValue(TypedTemplate))
      let overloads = overloads(scope, ex.address.identifier, +typedTemplateType)
      if overloads.len != 0:
        let templ = overloads[0]
        var arguments = newSeq[Statement](ex.arguments.len + 1)
        arguments[0] = constant(scope, makeType(Scope))
        for i in 0 ..< ex.arguments.len:
          arguments[i + 1] = Statement(kind: skConstant,
            cachedType: makeType(Statement),
            constant: Value(kind: vkStatement, statementValue: map(ex.arguments[i])))
        let call = Statement(kind: skFunctionCall,
          callee: variableGet(templ),
          arguments: arguments).toInstruction
        result = scope.context.evaluateStatic(call).statementValue
    if result.isNil:
      var arguments = newSeq[Statement](ex.arguments.len)
      var argumentTypes = toRef(newSeq[Type](ex.arguments.len))
      for i in 0 ..< arguments.len:
        let b =
          if bound.boundType.kind == tyFunction and bound.variance.matches:
            bound.boundType.arguments[i] * bound.variance
          else:
            defaultBound
        arguments[i] = map(ex.arguments[i], b)
        argumentTypes[][i] = arguments[i].cachedType
      var functionType = Type(kind: tyFunction,
        returnType: toRef makeType(None),
        arguments: argumentTypes)
      # XXX dynamic overloading
      let callee = map(ex.address, -functionType)
      result = Statement(kind: skFunctionCall,
        cachedType: callee.cachedType.returnType[],
        callee: callee,
        arguments: arguments)
  of Subscript:
    # what specialization can go here
    result = forward(Expression(kind: PathCall,
      address: Expression(kind: Symbol, identifier: ".[]"),
      arguments: @[ex.address] & ex.arguments))
  of CurlySubscript:
    result = forward(Expression(kind: PathCall,
      address: Expression(kind: Symbol, identifier: ".{}"),
      arguments: @[ex.address] & ex.arguments))
  of Colon:
    assert false, "cannot compile lone colon expression"
  of Comma, Tuple:
    if bound.boundType.kind == tyTuple and bound.variance.matches:
      assert bound.boundType.elements[].len == ex.elements.len, "tuple bound type lengths do not match"
    result = Statement(kind: skTuple, cachedType:
      Type(kind: tyTuple, elements: toRef(newSeqOfCap[Type](ex.elements.len))))
    for i, e in ex.elements:
      let element = if bound.boundType.kind == tyTuple and bound.variance.matches:
        map(e, bound.boundType.elements[][i] * bound.variance)
      else:
        map e
      result.elements.add(element)
      result.cachedType.elements[].add(element.cachedType)
  of ExpressionKind.Array:
    result = Statement(kind: skList, cachedType: makeType(List))
    var boundSet = bound.boundType.kind == tyList and bound.variance.matches 
    var b =
      if boundSet:
        bound.boundType.elementType[] * bound.variance
      else:
        defaultBound
    for e in ex.elements:
      let element = map(e, b)
      result.elements.add(element)
      if result.cachedType.elementType.isNil or result.cachedType.elementType[] < element.cachedType:
        result.cachedType.elementType = toRef(element.cachedType)
      if not boundSet:
        b = +element.cachedType
        boundSet = true
  of Set:
    proc isSingleColon(e: Expression): bool =
      e.kind == Symbol and e.identifier == ":"
    if ex.elements.len != 0 and (ex.elements[0].kind == Colon or
      ex.elements[0].isSingleColon):
      result = Statement(kind: skTable, cachedType: makeType(Table))
      var boundSet = bound.boundType.kind == tyList and bound.variance.matches 
      var (bk, bv) =
        if boundSet:
          (bound.boundType.keyType[] * bound.variance,
            bound.boundType.valueType[] * bound.variance)
        else:
          (defaultBound, defaultBound)
      for e in ex.elements:
        if e.isSingleColon: continue
        assert e.kind == Colon, "table literal must only have colon expressions"
        let k = map(e.left, bk)
        let v = map(e.right, bv)
        result.entries.add((key: k, value: v))
        if result.cachedType.keyType.isNil or result.cachedType.keyType[] < k.cachedType:
          result.cachedType.keyType = toRef(k.cachedType)
        if result.cachedType.valueType.isNil or result.cachedType.valueType[] < v.cachedType:
          result.cachedType.valueType = toRef(v.cachedType)
        if not boundSet:
          bk = +k.cachedType
          bv = +v.cachedType
          boundSet = true
    else:
      result = Statement(kind: skSet, cachedType: makeType(Set))
      var boundSet = bound.boundType.kind == tySet and bound.variance.matches 
      var b =
        if boundSet:
          bound.boundType.elementType[] * bound.variance
        else:
          defaultBound
      for e in ex.elements:
        let element = map(e, b)
        result.elements.add(element)
        if result.cachedType.elementType.isNil or result.cachedType.elementType[] < element.cachedType:
          result.cachedType.elementType = toRef(element.cachedType)
        if not boundSet:
          b = +element.cachedType
          boundSet = true
  of Block, SemicolonBlock:
    result = Statement(kind: skSequence)
    for i, e in ex.statements:
      let b =
        if i == ex.statements.high:
          bound
        else:
          -makeType(None) # like void
      let element = map(e, bound = b)
      result.sequence.add(element)
      if i == ex.statements.high:
        result.cachedType = element.cachedType
  if not bound.matchBound(result.cachedType):
    raise (ref TypeBoundMatchError)(
      expression: ex,
      bound: bound,
      type: result.cachedType,
      msg: "bound " & $bound & " does not match type " & $result.cachedType &
       " in expression " & $ex)

type Program* = Function #[object
  # this is just function
  stack*: Stack
  instruction*: Instruction]#

proc compile*(ex: Expression, imports: seq[Context]): Program =
  new(result)
  var context = newContext(imports)
  result.instruction = compile(context.top, ex, -makeType(None)).toInstruction
  context.refreshStack()
  result.stack = context.stack.shallowRefresh()

proc run*(program: Program, effectHandler: EffectHandler = nil): Value =
  evaluate(program.instruction, program.stack, effectHandler)
