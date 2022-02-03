import primitives, ../language/[expressions, number], std/[tables, sets, strutils]

type
  Variable* = ref object
    name*: string
    `type`*: Type
    stackIndex*: int
    defaultExpression*: Expression

  Context* {.acyclic.} = ref object
    ## current module or function
    imports*: seq[Context]
    stackSize*: int
    stack*: Stack
    variables*: seq[Variable]
  
  VariableAddress* = object
    ## address of variable relative to scope
    indices*: seq[int]

  VariableReference* = object
    variable*: Variable
    address*: VariableAddress
  
  Scope* {.acyclic.} = ref object
    ## restricted subset of variables in a context
    #imports*: seq[Scope]
    parent*: Scope
    context*: Context
    variables*: seq[Variable]
    #accumStackSize*: int
    #  ## the stack size from before the start of this scope
  
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

type TypeMatch* = enum
  # in order of strength
  tmNone, tmFalse, tmTrue, tmEqual

static: assert ord(high(TypeMatch)) mod 2 == 1, "type match enum must be even"

const
  highestNonMatching* = tmFalse
  lowestMatching* = tmTrue

static: assert lowestMatching == succ(highestNonMatching)

proc invert*(tm: TypeMatch): TypeMatch =
  if tm in {tmNone, tmEqual}: return tm
  TypeMatch(ord(high(TypeMatch)) - ord(tm))

proc matches*(tm: TypeMatch): bool =
  tm >= lowestMatching

template boolMatch*(b: bool): TypeMatch =
  if b: tmTrue else: tmFalse

proc match*(matchType, t: Type): TypeMatch =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  proc reduceMatch(s1, s2: seq[Type]): TypeMatch =
    if s1.len != s2.len: return low(TypeMatch)
    result = high(TypeMatch)
    for i in 0 ..< s1.len:
      let m = match(s1[i], s2[i])
      if m == low(TypeMatch): return m
      elif m < result: result = m
  if matchType == t: return tmEqual
  result = case matchType.kind
  of concreteTypeKinds:
    if matchType.kind != t.kind: return tmNone
    case matchType.kind
    of tyNoneValue, tyInteger, tyUnsigned, tyFloat, tyString:
      tmEqual
    of tyReference, tyList, tySet:
      match(matchType.elementType[], t.elementType[])
    of tyTuple:
      reduceMatch(matchType.elements[], t.elements[])
    of tyFunction:
      let rm = match(matchType.returnType[], t.returnType[])
      #if rm in {tmNone, tmFalse}:
      #  return rm
      let am = reduceMatch(matchType.arguments[], t.arguments[])
      min(am, rm)
    of tyTable:
      min(
        match(matchType.keyType[], t.keyType[]),
        match(matchType.valueType[], t.valueType[]))
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatch =
        if t1.len != t2.len: return low(TypeMatch)
        for k, v1 in t1:
          if k notin t2: return low(TypeMatch)
          let m = match(v1, t2[k])
          if m == low(TypeMatch): return m
          elif m < result: result = m
      tableMatch(matchType.fields, t.fields)
    of tyUnique:
      tmNone
    of tyType:
      match(matchType.typeValue[], t.typeValue[])
    of allTypeKinds - concreteTypeKinds: tmNone # unreachable
  of tyAny: tmTrue
  of tyNone: tmFalse
  of tyUnion:
    for a in matchType.operands[]:
      if match(a, t).matches:
        return tmTrue
    tmFalse
  of tyIntersection:
    for a in matchType.operands[]:
      if not match(a, t).matches:
        return tmFalse
    tmTrue
  of tyBaseType:
    boolMatch t.kind == matchType.baseKind
  of tyCustomMatcher:
    boolMatch matchType.typeMatcher(t)
  of tyContainingProperty:
    boolMatch matchType.containingProperty in t.properties
  of tyNot:
    const mapping = [
      tmNone: tmTrue,
      tmFalse: tmTrue,
      tmTrue: tmFalse,
      tmEqual: tmFalse]
    mapping[match(matchType.notType[], t)]

proc compare*(t1, t2: Type): int =
  ## t1 < t2 mirrors being a subtype
  let
    m1 = t1.match(t2)
    m2 = t2.match(t1)
  assert not (m1 == tmEqual and m1 != m2), "equal match must be commutative"
  if m1 == m2:
    return 0
  case m1
  of tmFalse:
    case m2
    of tmNone: return -1
    of tmTrue: return -2
    else: discard # unreachable
  of tmTrue:
    case m2
    of tmNone: return 1
    of tmFalse: return 2
    else: discard # unreachable
  of tmNone:
    case m2
    of tmTrue: return -1
    of tmFalse: return 1
    else: discard # unreachable
  else: discard # unreachable
  assert false, "unreachable, match kinds " & $m1 & " " & $m2

template `<`*(a, b: Type): bool = compare(a, b) < 0
template `<=`*(a, b: Type): bool = compare(a, b) <= 0
template `>`*(a, b: Type): bool = compare(a, b) > 0
template `>=`*(a, b: Type): bool = compare(a, b) >= 0

type
  TypeBound* = object
    boundType*: Type
    variance*: TypeMatch
  
  CompileError* = object of CatchableError

  TypeBoundMatchError* = object of CompileError
    bound*: TypeBound
    `type`*: Type
  
  NoOverloadFoundError* = object of CompileError
    bound*: TypeBound
    expression*: Expression
    scope*: Scope

proc matchBound*(b: TypeBound, t: Type): bool =
  if b.variance.matches:
    b.boundType.match(t) >= b.variance
  else:
    b.boundType.match(t) <= b.variance

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
    if name == v.name and bound.matchBound(v.type):
      result.add(VariableReference(variable: v, address: VariableAddress(indices: @[v.stackIndex])))

import algorithm

proc overloads*(scope: Scope | Context, name: string, bound: TypeBound): seq[VariableReference] =
  result = symbols(scope, name, bound)
  # sort must be stable
  result.sort(
    cmp = proc (a, b: VariableReference): int =
      compare(a.variable.type, b.variable.type),
    order = if bound.variance.matches: Descending else: Ascending)
  result.reverse()

proc compile*(scope: Scope, ex: Expression, bound: TypeBound): Statement =
  let defaultBound {.global.} = TypeBound(boundType: Type(kind: tyAny), variance: tmTrue)
  template map(ex: Expression, bound = defaultBound): Statement =
    compile(scope, ex, bound)
  case ex.kind
  of None: result = Statement(kind: skNone, cachedType: Type(kind: tyNone))
  of Number:
    let s = $ex.number
    result = Statement(kind: skConstant)
    case ex.number.kind
    of Integer:
      let val = parseInt(s)
      if bound.boundType.kind == tyFloat:
        result.constant = toValue(val.float)
        result.cachedType = Type(kind: tyFloat)
      elif val >= 0 and bound.boundType.kind == tyUnsigned:
        result.constant = toValue(val.uint)
        result.cachedType = Type(kind: tyUnsigned)
      else:
        result.constant = toValue(val)
        result.cachedType = Type(kind: tyInteger)
    of Floating:
      result.constant = toValue(parseFloat(s))
      result.cachedType = Type(kind: tyFloat)
    of Unsigned:
      result.constant = toValue(parseUInt(s))
      result.cachedType = Type(kind: tyUnsigned)
  of String:
    result = Statement(kind: skConstant, constant: toValue(ex.str), cachedType: Type(kind: tyString))
  of Wrapped:
    result = map(ex.wrapped, bound)
  of Name, Symbol:
    let overloads = overloads(scope, ex.identifier, bound)
    if overloads.len == 0:
      let exc = newException(NoOverloadFoundError, "no overloads with bound " & $bound & " for " & $ex)
      exc.bound = bound
      exc.expression = ex
      raise exc
    let r = overloads[0]
    let t = r.variable.type
    result = Statement(kind: skVariableGet,
      variableGetIndex: r.address.indices[0],
      cachedType: t)
    for i in 1 ..< r.address.indices.len:
      result = Statement(kind: skFromImportedStack,
        importedStackIndex: r.address.indices[i],
        importedStackStatement: result,
        cachedType: t)
  of Dot:
    var compiled = false
    if ex.right.kind == Name:
      let lhs = map ex.left
      let name = ex.right.identifier
      if lhs.cachedType.kind == tyComposite and lhs.cachedType.fields.hasKey(name):
        result = Statement(kind: skFunctionCall,
          cachedType: lhs.cachedType.fields[name],
          callee: Statement(kind: skConstant,
            constant: Value(kind: vkNativeFunction, nativeFunctionValue: proc (args: openarray[Value]): Value {.nimcall.} =
              args[0].compositeValue[args[1].stringValue[]])),
          arguments: @[lhs, Statement(kind: skConstant, constant: Value(kind: vkString, stringValue: toRef(name)))])
      else:
        try:
          result = map(Expression(kind: PathCall,
            address: Expression(kind: Symbol, identifier: "." & name),
            arguments: @[ex.left]), bound)
          compiled = true
        except CompileError: discard
    if not compiled:
      result = map(Expression(kind: PathCall,
        address: Expression(kind: Symbol, identifier: "."),
        arguments: @[ex.left, ex.right]), bound)
  of OpenCall, Infix, Prefix, Postfix, PathCall, PathInfix, PathPrefix, PathPostfix:
    discard # todo:
    # template (expression -> expression),
    # typechecker (expression -> statement),
    # typedtemplate (statement -> statement),
    # instructor (instruction -> value),
    # function (value -> value)
    # find a comfortable way to attach these to signatures of normal functions
  of Subscript:
    # generics specialization here maybe
    result = map(Expression(kind: PathCall,
      address: Expression(kind: Symbol, identifier: "[]"),
      arguments: @[ex.address] & ex.arguments), bound)
  of CurlySubscript:
    result = map(Expression(kind: PathCall,
      address: Expression(kind: Symbol, identifier: "{}"),
      arguments: @[ex.address] & ex.arguments), bound)
  of Colon:
    assert false, "cannot compile lone colon expression"
  of Comma, Tuple:
    # todo use bound
    if bound.boundType.kind == tyTuple and bound.variance.matches:
      assert bound.boundType.elements[].len == ex.elements.len, "tuple bound type lengths do not match"
    result = Statement(kind: skTuple, cachedType:
      Type(kind: tyTuple, elements: toRef(newSeqOfCap[Type](ex.elements.len))))
    for i, e in ex.elements:
      let element = if bound.boundType.kind == tyTuple and bound.variance.matches:
        map(e, TypeBound(boundType: bound.boundType.elements[][i], variance: bound.variance))
      else:
        map e
      result.elements.add(element)
      result.cachedType.elements[].add(element.cachedType)
  of Array:
    # todo use bound
    result = Statement(kind: skList, cachedType: Type(kind: tyList))
    let b =
      if bound.boundType.kind == tyList and bound.variance.matches:
        TypeBound(boundType: bound.boundType.elementType[], variance: bound.variance)
      else:
        defaultBound
    for e in ex.elements:
      let element = map(e, b)
      result.elements.add(element)
      if result.cachedType.elementType.isNil or result.cachedType.elementType[] < element.cachedType:
        result.cachedType.elementType = toRef(element.cachedType)
  of Set:
    # todo use bound
    proc isSingleColon(e: Expression): bool =
      e.kind == Symbol and e.identifier == ":"
    if ex.elements.len != 0 and (ex.elements[0].kind == Colon or
      ex.elements[0].isSingleColon):
      result = Statement(kind: skTable, cachedType: Type(kind: tyTable))
      let (bk, bv) =
        if bound.boundType.kind == tyTable and bound.variance.matches:
          (TypeBound(boundType: bound.boundType.keyType[], variance: bound.variance),
            TypeBound(boundType: bound.boundType.valueType[], variance: bound.variance))
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
          TypeBound(boundType: bound.boundType.elementType[], variance: bound.variance)
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
