import "."/[primitives, arrays, runtime], ../language/[expressions, number], std/[tables, sets, strutils]

type
  Variable* = ref object
    name*: string
    cachedType*: Type
    stackIndex*: int
    scope*: Scope # needed for cross-module lazy?
    lazyStaticValue*: Expression

  Context* {.acyclic.} = ref object
    ## current module or function
    imports*: seq[Context]
    #stackSize*: int
    stack*: Stack
    variables*: seq[Variable] ## should not shrink
  
  Scope* {.acyclic.} = ref object
    ## restricted subset of variables in a context
    #imports*: seq[Scope]
    parent*: Scope
    context*: Context
    variables*: seq[Variable] ## should not shrink
    #accumStackSize*: int
    #  ## the stack size from before the start of this scope
  
  VariableAddress* = object
    ## address of variable relative to context
    indices*: seq[int]

  VariableReference* = object
    variable*: Variable
    address*: VariableAddress
  
  StatementKind* = enum
    skNone
    skConstant
    skFunctionCall
    skSequence
    # stack
    skVariableGet
    skVariableSet
    skFromImportedStack
    # goto
    skIf
    skWhile
    skDoUntil
    # effect, can emulate goto
    skEmitEffect
    skHandleEffect
    # collections
    skTuple
    skList
    skSet
    skTable

  Statement* = ref object
    ## typed/compiled expression
    cachedType*: Type
    case kind*: StatementKind
    of skNone: discard
    of skConstant:
      constant*: Value
    of skFunctionCall:
      callee*: Statement
      arguments*: seq[Statement]
    of skSequence:
      sequence*: seq[Statement]
    of skVariableGet:
      variableGetIndex*: int
    of skVariableSet:
      variableSetIndex*: int
      variableSetValue*: Statement
    of skFromImportedStack:
      importedStackIndex*: int
      importedStackStatement*: Statement
    of skIf:
      ifCond*, ifTrue*, ifFalse*: Statement
    of skWhile:
      whileCond*, whileBody*: Statement
    of skDoUntil:
      doUntilCond*, doUntilBody*: Statement
    of skEmitEffect:
      effect*: Statement
    of skHandleEffect:
      effectHandler*, effectBody*: Statement
    of skTuple, skList, skSet:
      elements*: seq[Statement]
    of skTable:
      entries*: seq[tuple[key, value: Statement]]

proc refreshStack*(context: Context) =
  ## grow static stack if new variables have been added
  ## recursively do the same for imports
  ## should be called after full compilation
  for im in context.imports:
    im.refreshStack()
  if context.variables.len != context.stack.stack.len:
    var newStack = newArray[Value](context.variables.len)
    for i in 0 ..< context.stack.stack.len:
      newStack[i] = context.stack.stack[i]
    context.stack.stack = newStack

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
  case st.kind
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
  TypeMatch* = enum
    # in order of strength
    tmNone, tmFalse, tmTrue, tmAlmostEqual, tmEqual
  
  TypeBound* = object
    boundType*: Type
    variance*: TypeMatch

const
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

proc matches*(tm: TypeMatch): bool {.inline.} =
  tm >= lowestMatching

proc boolMatch*(b: bool): TypeMatch {.inline.} =
  if b: tmTrue else: tmFalse

proc match*(matcher, t: Type): TypeMatch =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  # properties do not have effect on default types besides dropping equal to almost equal
  proc reduceMatch(s1, s2: seq[Type]): TypeMatch =
    if s1.len != s2.len: return low(TypeMatch)
    result = pred(tmEqual)
    for i in 0 ..< s1.len:
      let m = match(s1[i], s2[i])
      if m == low(TypeMatch): return m
      elif m < result: result = m
  if matcher == t: return tmEqual
  result = case matcher.kind
  of concreteTypeKinds:
    if matcher.kind != t.kind: return tmNone
    case matcher.kind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat, tyString:
      tmAlmostEqual
    of tyReference, tyList, tySet:
      match(matcher.elementType[], t.elementType[])
    of tyTuple:
      reduceMatch(matcher.elements[], t.elements[])
    of tyFunction:
      let rm = match(matcher.returnType[], t.returnType[])
      #if rm in {tmNone, tmFalse}:
      #  return rm
      let am = reduceMatch(matcher.arguments[], t.arguments[])
      min(am, rm)
    of tyTable:
      min(
        match(matcher.keyType[], t.keyType[]),
        match(matcher.valueType[], t.valueType[]))
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatch =
        result = pred(tmEqual)
        if t1.len != t2.len: return low(TypeMatch)
        for k, v1 in t1:
          if k notin t2: return low(TypeMatch)
          let m = match(v1, t2[k])
          if m == low(TypeMatch): return m
          elif m < result: result = m
      tableMatch(matcher.fields, t.fields)
    of tyUnique:
      tmNone
    of tyType:
      match(matcher.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmNone # unreachable
  of tyAny: tmTrue
  of tyNone: tmFalse
  of tyUnion:
    for a in matcher.operands[]:
      if match(a, t).matches:
        return tmTrue
    tmFalse
  of tyIntersection:
    for a in matcher.operands[]:
      if not match(a, t).matches:
        return tmFalse
    tmTrue
  of tyNot:
    boolMatch not match(matcher.notType[], t).matches
  of tyBaseType:
    boolMatch t.kind == matcher.baseKind
  of tyCustomMatcher:
    boolMatch matcher.typeMatcher(t)
  of tyWithProperty:
    if matcher.withProperty notin t.properties:
      tmNone # could be tmFalse
    else:
      min(match(matcher.typeWithProperty[], t), pred(tmEqual))

proc compare*(t1, t2: Type): int =
  ## t1 < t2 mirrors being a subtype
  let
    m1 = t1.match(t2)
    m2 = t2.match(t1)
  assert not (m1 == tmEqual and m1 != m2), "equal match must be commutative"
  result = ord(m1) - ord(m2)

proc `<`*(a, b: Type): bool {.inline.} = compare(a, b) < 0
proc `<=`*(a, b: Type): bool {.inline.} = compare(a, b) <= 0
proc `>`*(a, b: Type): bool {.inline.} = compare(a, b) > 0
proc `>=`*(a, b: Type): bool {.inline.} = compare(a, b) >= 0

proc `+`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: lowestMatching)
proc `-`*(t: Type): TypeBound {.inline.} = TypeBound(boundType: t, variance: highestNonMatching)
proc `*`*(t: Type, variance: TypeMatch): TypeBound {.inline.} = TypeBound(boundType: t, variance: variance)

proc matchBound*(b: TypeBound, t: Type): bool {.inline.} =
  if b.variance.matches:
    b.boundType.match(t) >= b.variance
  else:
    b.boundType.match(t) <= b.variance

type
  CompileError* = object of CatchableError

  TypeBoundMatchError* = object of CompileError
    bound*: TypeBound
    `type`*: Type
  
  NoOverloadFoundError* = object of CompileError
    bound*: TypeBound
    expression*: Expression
    scope*: Scope

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement

proc setStatic*(variable: Variable, expression: Expression) =
  let value = variable.scope.compile(expression, +variable.cachedType)
  variable.cachedType = value.cachedType
  variable.scope.context.stack.set(variable.stackIndex, value.toInstruction.evaluate(variable.scope.context.stack))

proc isLazyUninitialized*(variable: Variable): bool =
  not variable.lazyStaticValue.isNil and variable.scope.context.stack.get(variable.stackIndex).kind == vkNone

proc getType*(variable: Variable): Type =
  if variable.isLazyUninitialized:
    variable.setStatic(variable.lazyStaticValue)
  variable.cachedType

proc symbols*(scope: Scope | Context, name: string, bound: TypeBound, doImports = true): seq[VariableReference] =
  if doImports:
    for i, im in (when scope is Scope: scope.context else: scope).imports:
      let addrs = symbols(im, name, bound)
      for a in addrs:
        var b = a
        b.address.indices.add(i)
        result.add(b)
  when scope is Scope:
    result.add(symbols(scope.parent, name, bound, doImports = false))
  for i, v in scope.variables:
    if name == v.name and bound.matchBound(v.getType()):
      result.add(VariableReference(variable: v, address: VariableAddress(indices: @[v.stackIndex])))

import algorithm

proc overloads*(scope: Scope | Context, name: string, bound: TypeBound): seq[VariableReference] =
  result = symbols(scope, name, bound)
  # sort must be stable
  result.sort(
    cmp = proc (a, b: VariableReference): int =
      compare(a.variable.cachedType, b.variable.cachedType),
    order = if bound.variance.matches: Ascending else: Descending)
  result.reverse()

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  let defaultBound {.global.} = +Type(kind: tyAny)
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
  of None: result = Statement(kind: skNone, cachedType: Type(kind: tyNone))
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
      let exc = newException(NoOverloadFoundError, "no overloads with bound " & $bound & " for " & $ex)
      exc.bound = bound
      exc.expression = ex
      raise exc
    let r = overloads[0]
    # XXX warn on ambiguity, thankfully recency is accounted for
    let t = r.variable.cachedType
    result = Statement(kind: skVariableGet,
      variableGetIndex: r.address.indices[0],
      cachedType: t)
    for i in 1 ..< r.address.indices.len:
      result = Statement(kind: skFromImportedStack,
        importedStackIndex: r.address.indices[i],
        importedStackStatement: result,
        cachedType: t)
  of Dot:
    if ex.right.kind == Name:
      let lhs = map ex.left
      let name = ex.right.identifier
      if lhs.cachedType.kind == tyComposite and lhs.cachedType.fields.hasKey(name):
        result = Statement(kind: skFunctionCall,
          cachedType: lhs.cachedType.fields[name],
          callee: constant(
            Value(kind: vkNativeFunction, nativeFunctionValue: proc (args: openarray[Value]): Value {.nimcall.} =
              args[0].compositeValue[args[1].stringValue[]]),
            tyNone),
          arguments: @[lhs, constant(name)])
      else:
        try:
          result = forward(
            Expression(kind: PathCall,
              address: Expression(kind: Symbol, identifier: "." & name),
              arguments: @[ex.left]))
        except CompileError: discard
    if result.isNil:
      result = forward(
        Expression(kind: PathCall,
          address: Expression(kind: Symbol, identifier: "."),
          arguments: @[ex.left, ex.right]))
  of OpenCall, Infix, Prefix, Postfix, PathCall, PathInfix, PathPrefix, PathPostfix:
    discard # todo:
    # template (expression -> expression),
    # typechecker (expression -> statement),
    # typedtemplate (statement -> statement),
    # instructor (instruction -> value),
    # function (value -> value)
    # find a comfortable way to attach these to signatures of normal functions
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
    result = Statement(kind: skList, cachedType: Type(kind: tyList))
    let b =
      if bound.boundType.kind == tyList and bound.variance.matches:
        bound.boundType.elementType[] * bound.variance
      else:
        defaultBound
    for e in ex.elements:
      let element = map(e, b)
      result.elements.add(element)
      if result.cachedType.elementType.isNil or result.cachedType.elementType[] < element.cachedType:
        result.cachedType.elementType = toRef(element.cachedType)
  of Set:
    proc isSingleColon(e: Expression): bool =
      e.kind == Symbol and e.identifier == ":"
    if ex.elements.len != 0 and (ex.elements[0].kind == Colon or
      ex.elements[0].isSingleColon):
      result = Statement(kind: skTable, cachedType: Type(kind: tyTable))
      let (bk, bv) =
        if bound.boundType.kind == tyTable and bound.variance.matches:
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
    else:
      result = Statement(kind: skSet, cachedType: Type(kind: tySet))
      let b =
        if bound.boundType.kind == tySet and bound.variance.matches:
          bound.boundType.elementType[] * bound.variance
        else:
          defaultBound
      for e in ex.elements:
        let element = map(e, b)
        result.elements.add(element)
        if result.cachedType.elementType.isNil or result.cachedType.elementType[] < element.cachedType:
          result.cachedType.elementType = toRef(element.cachedType)
  of Block, SemicolonBlock:
    result = Statement(kind: skSequence)
    for i, e in ex.statements:
      let b = if i == ex.statements.high: bound else: defaultBound
      let element = map(e, bound = b)
      result.sequence.add(element)
      if i == ex.statements.high:
        result.cachedType = element.cachedType
  if not bound.matchBound(result.cachedType):
    let ex = newException(TypeBoundMatchError, "bound " & $bound & " does not match type " & $result.cachedType)
    ex.bound = bound
    ex.type = result.cachedType
    raise ex
