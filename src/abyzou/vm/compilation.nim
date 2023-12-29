import
  std/[hashes, tables, sets, strutils],
  ../language/[expressions, number, shortstring],
  ./[ids, primitives, arrays, typebasics, typematch,
    valueconstr, checktype, guesstype, treewalk]

defineTypeBase Meta, TypeBase(name: "Meta",
  arguments: @[newTypeParameter("", +Type(kind: tyBase, typeBase: FunctionTy))])

proc newVariable*(name: string, knownType: Type = NoType): Variable =
  Variable(id: newVariableId(), name: name, nameHash: name.hash, knownType: knownType)

proc newContext*(parent: Scope = nil, imports: seq[Scope] = @[]): Context =
  result = Context(origin: parent)
  result.top = Scope(context: result, imports: imports)

proc childContext*(scope: Scope): Context =
  result = newContext(parent = scope)

proc childScope*(scope: Scope): Scope =
  result = Scope(parent: scope, context: scope.context)

proc makeStack*(context: Context): Stack =
  result = Stack(stack: newArray[Value](context.stackSlots.len))
  for i in 0 ..< context.stackSlots.len:
    result.stack[i] = context.stackSlots[i].value

proc fillStack(context: Context, stack: Stack) =
  for i in 0 ..< stack.stack.len:
    context.stackSlots[i].value = stack.stack[i]

template withStack*(context: Context, body) =
  let stack {.inject.} = makeStack(context)
  body
  fillStack(context, stack)

proc evaluateStatic*(context: Context, instr: Instruction): Value =
  context.withStack:
    result = instr.evaluate(stack)

proc define*(scope: Scope, variable: Variable) =
  variable.scope = scope
  variable.stackIndex = scope.context.stackSlots.len
  scope.context.stackSlots.add(
    StackSlot(kind: Local, variable: variable))
  scope.variables.add(variable)

proc set*(context: Context, variable: Variable, value: sink Value) =
  assert variable.scope.context == context
  context.stackSlots[variable.stackIndex].value = value

proc toInstruction*(st: Statement): Instruction =
  template map[T](s: T): T =
    s
  template map(s: Statement): Instruction =
    s.toInstruction
  template map[T, U](s: (T, U)): untyped =
    (map s[0], map s[1])
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
  of skDispatch:
    Instruction(kind: Dispatch,
      dispatchFunctions: map st.dispatchees,
      dispatchArguments: map st.dispatchArguments)
  of skSequence: Instruction(kind: Sequence, sequence: map st.sequence)
  of skVariableGet:
    Instruction(kind: VariableGet, variableGetIndex: st.variableGetIndex)
  of skVariableSet:
    Instruction(kind: VariableSet, variableSetIndex: st.variableSetIndex,
      variableSetValue: map st.variableSetValue)
  of skArmStack:
    Instruction(kind: ArmStack, armStackFunction: map st.armStackFunction,
      armStackCaptures: map st.armStackCaptures)
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
  of skGetIndex:
    Instruction(kind: GetIndex, getIndexAddress: map st.getIndexAddress,
      getIndex: st.getIndex)
  of skSetIndex:
    Instruction(kind: SetIndex, setIndexAddress: map st.setIndexAddress,
      setIndex: st.setIndex,
      setIndexValue: map st.setIndexValue)
  of skUnaryInstruction:
    Instruction(kind: st.unaryInstructionKind, unary: map st.unary)
  of skBinaryInstruction:
    Instruction(kind: st.binaryInstructionKind, binary1: map st.binary1, binary2: map st.binary2)

type
  CompileError* = object of CatchableError
    expression*: Expression

  TypeBoundMatchError* = object of CompileError
    bound*: TypeBound
    `type`*: Type
  
  NoOverloadFoundError* = object of CompileError
    bound*: TypeBound
    scope*: Scope
  
  CannotCallError* = object of NoOverloadFoundError
    `type`*: Type
    argumentTypes*: seq[Type]
  
  OutOfScopeModifyError* = object of CompileError
    variable*: Variable
    referenceKind*: VariableReferenceKind

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement

proc evaluateStatic*(scope: Scope, ex: Expression, bound: TypeBound = +AnyTy): Value =
  scope.context.evaluateStatic(scope.compile(ex, bound).toInstruction)

proc setStatic*(variable: Variable, expression: Expression) =
  let value = variable.scope.compile(expression, +variable.knownType)
  variable.knownType = value.knownType
  variable.scope.context.withStack:
    stack.set(variable.stackIndex, value.toInstruction.evaluate(stack))
  variable.evaluated = true

proc getType*(variable: Variable): Type =
  if variable.knownType.isNoType and not variable.lazyExpression.isNil and not variable.evaluated:
    variable.setStatic(variable.lazyExpression)
  variable.knownType

proc shallowReference*(v: Variable, `type`: Type = v.getType): VariableReference {.inline.} =
  VariableReference(variable: v, type: `type`, kind: Local)

proc symbols*(scope: Scope, name: string, bound: TypeBound,
  nameHash = name.hash): seq[VariableReference] =
  if scope.isNil: return
  if not scope.parent.isNil:
    result.add(symbols(scope.parent, name, bound, nameHash = nameHash))
  elif not scope.context.origin.isNil:
    for a in symbols(scope.context.origin, name, bound, nameHash = nameHash):
      let b =
        if a.kind == Constant:
          a
        else:
          VariableReference(variable: a.variable, type: a.type,
            kind: Capture, captureIndex: -1)
      result.add(b)
  for i, im in scope.imports:
    let addrs = symbols(im, name, bound, nameHash = nameHash)
    for a in addrs:
      result.add(VariableReference(variable: a.variable, type: a.type,
        kind: Constant))
  for i, v in scope.variables:
    if (v.nameHash == 0 or v.nameHash == nameHash) and name == v.name:
      var t = v.getType
      if v.genericParams.len != 0:
        var matches = ParameterInstantiation(strict: true,
          table: initTable[TypeParameter, Type](v.genericParams.len))
        try:
          discard match(t, bound.boundType, matches)
        except ParameterMatchError:
          matches.table.clear()
        if matches.table.len != 0:
          fillParameters(t, matches)
      if bound.matchBound(t):
        result.add(v.shallowReference(t))

import algorithm

proc overloads*(scope: Scope, name: string, bound: TypeBound): seq[VariableReference] =
  result = symbols(scope, name, bound)
  # sort must be stable to preserve definition/import order
  result.sort(
    cmp = proc (a, b: VariableReference): int =
      compare(a.type, b.type),
    order = if bound.variance == Covariant: Ascending else: Descending)
  result.reverse()

proc capture*(c: Context, v: Variable): int =
  if v.scope.context == c:
    result = v.stackIndex
  else:
    if not c.origin.isNil:
      discard c.origin.context.capture(v)
    result = c.captures.mgetOrPut(v, c.stackSlots.len)
    c.stackSlots.add(
      StackSlot(kind: Capture, variable: v,
        value: v.scope.context.stackSlots[v.stackIndex].value))

proc variableGet*(c: Context, r: VariableReference): Statement =
  let t = r.type
  case r.kind
  of Constant:
    result = Statement(kind: skConstant,
      constant: r.variable.scope.context.stackSlots[r.variable.stackIndex].value,
      knownType: t)
  of Local:
    result = Statement(kind: skVariableGet,
      variableGetIndex: r.variable.stackIndex,
      knownType: t)
  of Capture:
    result = Statement(kind: skVariableGet,
      variableGetIndex: c.capture(r.variable),
      knownType: t)

proc variableSet*(c: Context, r: VariableReference, value: Statement, source: Expression = nil): Statement =
  let t = r.type
  case r.kind
  of Local:
    result = Statement(kind: skVariableSet,
      variableSetIndex: r.variable.stackIndex,
      variableSetValue: value,
      knownType: t)
  of Constant, Capture:
    raise (ref OutOfScopeModifyError)(expression: source,
      variable: r.variable, referenceKind: r.kind,
      msg: "cannot modify " &
        (if r.kind == Capture: "captured " else: "constant ") &
        r.variable.name & ", use a reference instead")

template constant*(value: Value, ty: Type): Statement =
  Statement(kind: skConstant, constant: value, knownType: ty)
template constant*(value: untyped, ty: Type): Statement =
  constant(toValue(value), ty)
template constant*(value: string): Statement = constant(value, StringTy)
template constant*(value: int32): Statement = constant(value, Int32Ty)
template constant*(value: uint32): Statement = constant(value, Uint32Ty)
template constant*(value: float32): Statement = constant(value, Float32Ty)

proc isNative(bound: TypeBound, nt: NativeType): bool {.inline.} =
  bound.boundType.kind == tyCompound and bound.boundType.base.nativeType == nt

proc compileNumber*(ex: Expression, bound: TypeBound): Statement =
  let s = $ex.number
  result = Statement(kind: skConstant)
  case ex.number.kind
  of Integer:
    let val = parseInt(s)
    if isNative(bound, ntyFloat32):
      result = constant(val.float32)
    elif val >= 0 and isNative(bound, ntyUint32):
      result = constant(val.uint32)
    else:
      result = constant(val.int32)
  of Floating:
    result = constant(parseFloat(s).float32)
  of Unsigned:
    result = constant(parseUInt(s).uint32)

template defaultBound: untyped = +AnyTy
template map(ex: Expression, bound = defaultBound): Statement =
  compile(scope, ex, bound)
template forward(ex: Expression): Statement =
  compile(scope, ex, bound)
proc isSingleColon(e: Expression): bool =
  e.kind == Symbol and e.symbol == short":"

proc resolve*(scope: Scope, ex: Expression, name: string, bound: TypeBound): VariableReference =
  let overloads = overloads(scope, name, bound)
  if overloads.len == 0:
    raise (ref NoOverloadFoundError)(
      expression: ex,
      bound: bound,
      scope: scope,
      msg: "no overloads with bound " & $bound & " for " & $ex)
  result = overloads[0]
  when false:
    # xxx can implement generic evaluation here
    # maybe lazyExpression compiled with a new scope with the type variables
    # but then we would need to save every version
    # can generate new variables but that would fill up scope
    if result.variable.genericParams.len != 0:
      var matches: ParameterInstantiation = initTable[TypeParameter, Type](result.variable.genericParams.len)
      try:
        matchParameters(result.type, bound.boundType, matches)
      except GenericMatchError as e:
        e.expression = ex
        raise e
      block:
        var unmatchedParams: seq[TypeParameter]
        for p in result.variable.genericParams:
          if p notin matches:
            unmatchedParams.add(p)
        if unmatchedParams.len != 0:
          raise (ref GenericUnmatchedError)(
            expression: ex,
            allParameters: result.variable.genericParams,
            matchedParameters: matches)
  if not bound.matchBound(result.type):
    raise (ref TypeBoundMatchError)(
      expression: ex,
      bound: bound,
      type: result.type,
      msg: "bound " & $bound & " does not match type " & $result.type &
       " in expression " & $ex)

proc compileCall*(scope: Scope, ex: Expression, bound: TypeBound,
  argumentStatements: sink seq[Statement] = newSeq[Statement](ex.arguments.len)): Statement

template same(a, b: Expression): bool = system.`==`(a, b)

proc compileProperty*(scope: Scope, ex: Expression, lhs: Statement, name: string, bound: TypeBound): Statement =
  result = nil
  if lhs.knownType.kind == tyTuple and lhs.knownType.elementNames.contains(name):
    let ind = lhs.knownType.elementNames[name]
    result = Statement(kind: skGetIndex,
      knownType: lhs.knownType.nth(ind),
      getIndexAddress: lhs,
      getIndex: ind)
  else:
    let ident = Expression(kind: Name, identifier: "." & name)
    try:
      result = forward(
        Expression(kind: PathCall,
          address: ident,
          arguments: @[ex.left]))
    except NoOverloadFoundError as e:
      if not same(ident, e.expression):
        raise

proc compileMetaCall*(scope: Scope, name: string, ex: Expression, bound: TypeBound, argumentStatements: var seq[Statement]): Statement =
  result = nil
  var argumentTypes = newSeq[Type](ex.arguments.len)
  for t in argumentTypes.mitems: t = AnyTy
  # XXX (7) pass type bound as well as scope, to pass both to a compile proc
  # maybe by passing a meta call context object
  # XXX (7) validate output statement
  var realArgumentTypes = newSeq[Type](ex.arguments.len + 1)
  realArgumentTypes[0] = ScopeTy
  for i in 1 ..< realArgumentTypes.len:
    realArgumentTypes[i] = union(ExpressionTy, StatementTy)
  # get all metas first and type statements accordingly
  var allMetas = overloads(scope, name,
    *funcType(StatementTy, realArgumentTypes).withProperties(
      property(Meta, funcType(if bound.variance == Covariant: AnyTy else: bound.boundType, argumentTypes))))
  if allMetas.len != 0:
    var makeStatement = newSeq[bool](ex.arguments.len)
    var metaTypes = newSeq[Type](allMetas.len)
    for i, v in allMetas:
      let ty = v.type
      let metaTy = v.type.properties[Meta].baseArguments[0]
      metaTypes[i] = metaTy
      for i in 0 ..< ex.arguments.len:
        if matchBound(+StatementTy, ty.param(i + 1)):
          makeStatement[i] = true
          # this should be correct but it was commonSubType before
          argumentTypes[i] = commonSuperType(argumentTypes[i], metaTy.param(i))
    for i, x in makeStatement:
      if x:
        try:
          argumentStatements[i] = map(ex.arguments[i], +argumentTypes[i])
        except NoOverloadFoundError as e:
          if same(e.expression, ex.arguments[i]):
            reset(allMetas)
            break
        except TypeBoundMatchError as e:
          if same(e.expression, ex.arguments[i]):
            reset(allMetas)
            break
        argumentTypes[i] = commonSubType(argumentTypes[i], argumentStatements[i].knownType)
    var superMetas, subMetas: typeof(allMetas)
    for i, m in allMetas:
      let mt = metaTypes[i]
      if matchBound(+mt,
        funcType(if bound.variance == Covariant: AnyTy else: bound.boundType, argumentTypes)):
        superMetas.add(m)
      if matchBound(
        +funcType(if bound.variance == Covariant: NoneTy else: bound.boundType, argumentTypes),
        mt):
        subMetas.add(m)
    superMetas.sort(
      cmp = proc (a, b: VariableReference): int =
        compare(a.type, b.type),
      order = Descending)
    subMetas.sort(
      cmp = proc (a, b: VariableReference): int =
        compare(a.type, b.type),
      order = Ascending)
    if superMetas.len != 0:
      let meta = superMetas[0]
      let ty = meta.type
      var arguments = newSeq[Statement](ex.arguments.len + 1)
      arguments[0] = constant(scope, ScopeTy)
      for i in 0 ..< ex.arguments.len:
        if matchBound(+StatementTy, ty.param(i + 1)):
          arguments[i + 1] = constant(argumentStatements[i], StatementTy)
        else:
          arguments[i + 1] = constant(copy ex.arguments[i], ExpressionTy)
      let call = Statement(kind: skFunctionCall,
        callee: variableGet(scope.context, meta),
        arguments: arguments).toInstruction
      result = scope.context.evaluateStatic(call).boxedValue.statementValue
    else:
      for d in subMetas:
        var arguments = newArray[Value](ex.arguments.len + 1)
        arguments[0] = toValue scope
        for i in 0 ..< ex.arguments.len:
          if matchBound(+StatementTy, d.type.param(i + 1)):
            arguments[i + 1] = toValue argumentStatements[i]
          else:
            arguments[i + 1] = toValue copy ex.arguments[i]
        if checkType(toValue arguments, d.type.baseArguments[0]):
          var argumentStatement = newSeq[Statement](arguments.len)
          for i, a in arguments: argumentStatement[i] = constant(a, a.getType)
          let call = Statement(kind: skFunctionCall,
            callee: variableGet(scope.context, d),
            arguments: argumentStatement).toInstruction
          result = scope.context.evaluateStatic(call).boxedValue.statementValue
          break

proc compileRuntimeCall*(scope: Scope, ex: Expression, bound: TypeBound,
  argumentStatements: var seq[Statement],
  argumentTypes: var seq[Type], 
  functionType: var Type): Statement =
  result = nil
  for i in 0 ..< ex.arguments.len:
    if argumentStatements[i].isNil:
      argumentStatements[i] = map(ex.arguments[i])
    argumentTypes[i] = argumentStatements[i].knownType
  functionType = funcType(if bound.variance == Covariant: AnyTy else: bound.boundType, argumentTypes)
  # lowest supertype function:
  try:
    # XXX (2) named arguments
    var withConstructor = functionType
    withConstructor.baseArguments[0] = TupleConstructorTy[withConstructor.baseArguments[0]]
    let callee = map(ex.address, -withConstructor)
    reorderTupleConstructor(withConstructor.baseArguments[0].baseArguments[0],
      callee.knownType.baseArguments[0],
      argumentStatements)
    result = Statement(kind: skFunctionCall,
      knownType: callee.knownType.baseArguments[1],
      callee: callee,
      arguments: argumentStatements)
  except NoOverloadFoundError as e:
    # dispatch lowest subtype functions in order:
    if same(e.expression, ex.address) and ex.address.isIdentifier(name):
      functionType.baseArguments[1] =
        if bound.variance == Covariant:
          NoneTy
        else:
          bound.boundType
      let subs = overloads(scope, name, +functionType)
      if subs.len != 0:
        var dispatchees = newSeq[(seq[Type], Statement)](subs.len)
        for i, d in dispatchees.mpairs:
          let t = subs[i].type
          d[0].newSeq(argumentStatements.len)
          for i in 0 ..< argumentStatements.len:
            let pt = t.param(i)
            let m = match(-argumentTypes[i], pt)
            if m.matches:
              # optimize checking types we know match
              # XXX do this recursively using deep matches for some types
              d[0][i] = AnyTy
            else:
              d[0][i] = pt
          d[1] = variableGet(scope.context, subs[i])
        result = Statement(kind: skDispatch,
          knownType: functionType.baseArguments[1],
            # we could calculate a union here but it could become too complex
          dispatchees: dispatchees,
          dispatchArguments: argumentStatements)

proc compileCall*(scope: Scope, ex: Expression, bound: TypeBound,
  argumentStatements: sink seq[Statement] = newSeq[Statement](ex.arguments.len)): Statement =
  if ex.address.isIdentifier(name):
    # meta calls take evaluation priority in overloading with runtime calls
    result = compileMetaCall(scope, name, ex, bound, argumentStatements)
  if result.isNil:
    var argumentTypes = newSeq[Type](ex.arguments.len)
    var functionType: Type
    result = compileRuntimeCall(scope, ex, bound, argumentStatements, argumentTypes, functionType)
    assert not functionType.isNoType
    # .call, should recurse but compiled arguments should be reused:
    if result.isNil:
      let callee = map ex.address
      argumentStatements.insert(callee, 0)
      argumentTypes.insert(callee.knownType, 0)
      functionType = funcType(if bound.variance == Covariant: AnyTy else: bound.boundType, argumentTypes)
      let overs = overloads(scope, ".call", -functionType)
      if overs.len != 0:
        let dotCall = variableGet(scope.context, overs[0])
        result = Statement(kind: skFunctionCall,
          knownType: dotCall.knownType.baseArguments[1],
          callee: callee,
          arguments: argumentStatements)
      else:
        raise (ref CannotCallError)(
          expression: ex.address,
          bound: bound,
          scope: scope,
          argumentTypes: argumentTypes,
          type: callee.knownType,
          msg: "no way to call " & $ex.address & " of type " & $callee.knownType &
            " found for argument types " & $argumentTypes)

proc compileTupleExpression*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  # XXX tuple type sugar?
  if bound.boundType.kind == tyTuple:
    assert bound.boundType.elements.len == ex.elements.len, "tuple bound type lengths do not match"
  result = Statement(kind: skTuple, knownType: Type(kind: tyTuple, elements: newSeqOfCap[Type](ex.elements.len)))
  for i, e in ex.elements:
    var val: Expression
    if e.kind == Colon:
      assert e.left.kind == Name, "tuple field has non-name left hand side on colon expression"
      let name = e.left.identifier
      result.knownType.elementNames[name] = i
      val = e.right
    else:
      val = e
    let element = if bound.boundType.kind == tyTuple:
      map(val, bound.boundType.elements[i] * bound.variance)
    else:
      map val
    result.elements.add(element)
    result.knownType.elements.add(element.knownType)

proc compileArrayExpression*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  result = Statement(kind: skList, knownType: ListTy[AnyTy])
  var boundSet = isNative(bound, ntyList)
  var b =
    if boundSet:
      bound.boundType.baseArguments[0] * bound.variance
    else:
      defaultBound
  for e in ex.elements:
    let element = map(e, b)
    result.elements.add(element)
    if result.knownType.baseArguments[0].isNoType or result.knownType.baseArguments[0] < element.knownType:
      result.knownType.baseArguments[0] = element.knownType
    if not boundSet:
      b = +element.knownType
      boundSet = true

proc compileSetExpression*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  if ex.elements.len != 0 and (ex.elements[0].kind == Colon or
    ex.elements[0].isSingleColon):
    result = Statement(kind: skTable, knownType: TableTy[AnyTy, AnyTy])
    var boundSet = isNative(bound, ntyTable)
    var (bk, bv) =
      if boundSet:
        (bound.boundType.baseArguments[0] * bound.variance,
          bound.boundType.baseArguments[1] * bound.variance)
      else:
        (defaultBound, defaultBound)
    for e in ex.elements:
      if e.isSingleColon: continue
      assert e.kind == Colon, "table literal must only have colon expressions"
      let k = map(e.left, bk)
      let v = map(e.right, bv)
      result.entries.add((key: k, value: v))
      if result.knownType.baseArguments[0].isNoType or result.knownType.baseArguments[0] < k.knownType:
        result.knownType.baseArguments[0] = k.knownType
      if result.knownType.baseArguments[1].isNoType or result.knownType.baseArguments[1] < v.knownType:
        result.knownType.baseArguments[1] = v.knownType
      if not boundSet:
        bk = +k.knownType
        bv = +v.knownType
        boundSet = true
  else:
    result = Statement(kind: skSet, knownType: SetTy[AnyTy])
    var boundSet = isNative(bound, ntySet) 
    var b =
      if boundSet:
        bound.boundType.baseArguments[0] * bound.variance
      else:
        defaultBound
    for e in ex.elements:
      let element = map(e, b)
      result.elements.add(element)
      if result.knownType.baseArguments[0].isNoType or result.knownType.baseArguments[0] < element.knownType:
        result.knownType.baseArguments[0] = element.knownType
      if not boundSet:
        b = +element.knownType
        boundSet = true

proc compileBlock*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  result = Statement(kind: skSequence)
  for i, e in ex.statements:
    let b =
      if i == ex.statements.high:
        bound
      else:
        -NoneTy # like void
    let element = map(e, bound = b)
    result.sequence.add(element)
    if i == ex.statements.high:
      result.knownType = element.knownType

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  case ex.kind
  of None: result = Statement(kind: skNone, knownType: NoType)
  of Number: result = compileNumber(ex, bound)
  of String, SingleQuoteString: result = constant(ex.str)
  of Wrapped: result = forward(ex.wrapped)
  of Name, Symbol:
    # XXX warn on ambiguity, thankfully recency is accounted for
    let name = if ex.kind == Symbol: $ex.symbol else: ex.identifier
    result = variableGet(scope.context, resolve(scope, ex, name, bound))
  of Dot:
    if ex.right.kind == Name:
      let lhs = map ex.left
      let name = ex.right.identifier
      result = compileProperty(scope, ex, lhs, name, bound)
    if result.isNil:
      result = forward(
        Expression(kind: PathCall,
          address: Expression(kind: Name, identifier: "."),
          arguments: @[ex.left, ex.right]))
  of CallKinds: result = compileCall(scope, ex, bound)
  of Subscript:
    # XXX specialize for overloaded variables
    result = forward(Expression(kind: PathCall,
      address: newSymbolExpression(short".[]"),
      arguments: @[ex.address] & ex.arguments))
  of CurlySubscript:
    result = forward(Expression(kind: PathCall,
      address: newSymbolExpression(short".{}"),
      arguments: @[ex.address] & ex.arguments))
  of Colon:
    assert false, "cannot compile lone colon expression"
  of Comma, Tuple:
    result = compileTupleExpression(scope, ex, bound)
  of ExpressionKind.Array:
    result = compileArrayExpression(scope, ex, bound)
  of Set:
    result = compileSetExpression(scope, ex, bound)
  of Block, SemicolonBlock:
    result = compileBlock(scope, ex, bound)
  if not bound.matchBound(result.knownType):
    raise (ref TypeBoundMatchError)(
      expression: ex,
      bound: bound,
      type: result.knownType,
      msg: "bound " & $bound & " does not match type " & $result.knownType &
        " in expression " & $ex)

type Program* = TreeWalkFunction

proc compile*(ex: Expression, imports: seq[Scope], bound: TypeBound = +AnyTy): Program =
  var context = newContext(imports = imports)
  result.instruction = compile(context.top, ex, bound).toInstruction
  result.stack = makeStack(context)

proc run*(program: Program, effectHandler: EffectHandler = nil): Value =
  evaluate(program.instruction, program.stack, effectHandler)
