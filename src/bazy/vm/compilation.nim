import primitives, language/[expressions, number], std/[tables, sets, strutils]

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
  
  Scope* {.acyclic.} = ref object
    ## restricted subset of variables in a context
    imports*: seq[Scope]
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

type TypeMatchKind* = enum
  # in order of strength
  tmNone, tmFalse, tmTrue, tmEqual

template boolMatch*(b: bool): TypeMatchKind =
  if b: tmTrue else: tmFalse

proc match*(matchType, t: Type): TypeMatchKind =
  # commutativity rules:
  # must be commutative when equal
  # otherwise either order can give none, in which the non-none result matters
  # otherwise generally should be anticommutative, but this is not necessary
  proc reduceMatch(s1, s2: seq[Type]): TypeMatchKind =
    if s1.len != s2.len: return low(TypeMatchKind)
    result = high(TypeMatchKind)
    for i in 0 ..< s1.len:
      let m = match(s1[i], s2[i])
      if m == low(TypeMatchKind): return m
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
      reduceMatch(matchType.elements, t.elements)
    of tyFunction:
      let rm = match(matchType.returnType[], t.returnType[])
      #if rm in {tmNone, tmFalse}:
      #  return rm
      let am = reduceMatch(matchType.arguments, t.arguments)
      min(am, rm)
    of tyTable:
      min(
        match(matchType.keyType[], t.keyType[]),
        match(matchType.valueType[], t.valueType[]))
    of tyComposite:
      proc tableMatch[T](t1, t2: Table[T, Type]): TypeMatchKind =
        if t1.len != t2.len: return low(TypeMatchKind)
        for k, v1 in t1:
          if k notin t2: return low(TypeMatchKind)
          let m = match(v1, t2[k])
          if m == low(TypeMatchKind): return m
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
    for a in matchType.operands:
      if match(a, t) >= tmTrue:
        return tmTrue
    tmFalse
  of tyIntersection:
    for a in matchType.operands:
      if match(a, t) <= tmFalse:
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

proc getType*(st: Statement): Type = discard

proc compile*(scope: Scope, ex: Expression, bound: (Type, TypeMatchKind)): Statement =
  proc matchBound(b: (Type, TypeMatchKind), t: Type): bool =
    if b[1] <= tmFalse:
      b[0].match(t) <= b[1]
    else:
      b[0].match(t) >= b[1]
  template assertType(t: Type) =
    assert bound.matchBound(t), "bound " & $bound & " does not match type " & $t
  template map(ex: Expression, bound = (Type(kind: tyAny), tmTrue)): Statement =
    compile(scope, ex, bound)
  case ex.kind
  of None: result = Statement(kind: skNone)
  of Number:
    let s = $ex.number
    result = Statement(kind: skConstant, constant:
      case ex.number.kind
      of Integer:
        assertType Type(kind: tyInteger)
        toValue(parseInt(s))
      of Floating:
        assertType Type(kind: tyFloat)
        toValue(parseFloat(s))
      of Unsigned:
        assertType Type(kind: tyUnsigned)
        toValue(parseUInt(s)))
  of String:
    assertType Type(kind: tyString)
    result = Statement(kind: skConstant, constant: toValue(ex.str))
  of Wrapped:
    result = map(ex.wrapped, bound)
  of Name, Symbol:
    discard
  of Dot:
    discard
  of OpenCall, Infix, Prefix, Postfix, PathCall, PathInfix, PathPrefix, PathPostfix:
    discard
  of Subscript:
    discard
  of CurlySubscript:
    result = map(Expression(kind: PathCall,
      address: Expression(kind: Symbol, identifier: "{}"),
      arguments: @[ex.address] & ex.arguments), bound)
  of Colon:
    assert false, "cannot compile lone colon expression"
  of Comma, Tuple:
    # todo use bound
    result = Statement(kind: skTuple)
    for e in ex.elements:
      result.elements.add(map e)
  of Array:
    # todo use bound
    result = Statement(kind: skList)
    for e in ex.elements:
      result.elements.add(map e)
  of Set:
    # todo use bound
    proc isSingleColon(e: Expression): bool =
      e.kind == Symbol and e.identifier == ":"
    if ex.elements.len != 0 and (ex.elements[0].kind == Colon or
      ex.elements[0].isSingleColon):
      result = Statement(kind: skTable)
      for e in ex.elements:
        if e.isSingleColon: continue
        assert e.kind == Colon, "table literal must only have colon expressions"
        result.entries.add((key: map e.left, value: map e.right))
    else:
      result = Statement(kind: skSet)
      for e in ex.elements:
        result.elements.add(map e)
  of Block, SemicolonBlock:
    result = Statement(kind: skSequence)
    for i, e in ex.statements:
      if i == ex.statements.high:
        result.sequence.add(map(e, bound = bound))
      else:
        result.sequence.add(map e)
