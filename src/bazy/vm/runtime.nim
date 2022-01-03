import "."/[primitives, compilation, arrays], std/[sets, tables]

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

type EffectHandler* = proc (effect: Value): bool

template toNegatedBool*(val: Value): bool =
  val.integerValue == 0

template toBool*(val: Value): bool =
  val.integerValue != 0

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: EffectHandler): Value

template run(instr: Instruction, stack, effectHandler): Value =
  let val = evaluate(instr, stack, effectHandler)
  if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val)):
    return val
  val

proc call*(fun: Function, args: sink Array[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  let newStack = Stack(imports: fun.imports, stack: newArray[Value](fun.stackSize))
  for i in 0 ..< args.len:
    newStack.stack[i] = move args[i]
  result = run(fun.instruction, newStack, effectHandler)

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: proc (effect: Value): bool): Value =
  template run(instr; stack = stack; effectHandler = effectHandler): untyped =
    run(instr, stack, effectHandler)
  case ins.kind
  of NoOp:
    result = Value(kind: vkNone)
  of Constant:
    result = ins.constantValue
  of FunctionCall:
    let fn = run ins.function
    var args = newArray[Value](ins.arguments.len)
    for i in 0 ..< args.len:
      args[i] = run ins.arguments[i]
    case fn.kind
    of vkNativeFunction:
      result = fn.nativeFunctionValue(args.toOpenArray(0, args.len - 1))
    of vkFunction:
      result = fn.functionValue.call(args, effectHandler)
    else:
      discard # error
  of Sequence:
    for instr in ins.sequence:
      result = run instr
  of VariableGet:
    result = stack.stack[ins.variableGetIndex]
  of VariableSet:
    result = run ins.variableSetValue
    stack.stack[ins.variableSetIndex] = result
  of FromImportedStack:
    result = run(ins.importedStackInstruction, stack.imports[ins.importedStackIndex])
  of If:
    let cond = run ins.ifCondition
    if cond.toBool:
      result = run ins.ifTrue
    else:
      result = run ins.ifFalse
  of While:
    while (let cond = run ins.whileCondition; cond.toBool):
      result = run ins.whileTrue
  of DoUntil:
    while true:
      result = run ins.doUntilTrue
      let cond = run ins.doUntilCondition
      if cond.toBool:
        break
  of EmitEffect:
    result = Value(kind: vkEffect)
    new result.effectValue
    result.effectValue[] = run ins.effect
  of HandleEffect:
    let h = run ins.effectHandler
    var handler: proc (effect: Value): bool
    case h.kind
    of vkNativeFunction:
      let f = h.nativeFunctionValue
      handler = proc (effect: Value): bool =
        f([effect]).toBool
    of vkFunction:
      let f = h.functionValue
      handler = proc (effect: Value): bool =
        let val = f.call([effect].toArray)
        if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val)):
          return false
        val.toBool
    else:
      discard
    result = run(ins.effectEmitter, stack, handler)
  of BuildTuple:
    if ins.elements.len <= 255:
      var arr = newShortArray[Value](ins.elements.len)
      for i in 0 ..< arr.len:
        arr[i] = run ins.elements[i]
      result = toValue(arr)
    else:
      var arr = newArray[Value](ins.elements.len)
      for i in 0 ..< arr.len:
        arr[i] = run ins.elements[i]
      result = toValue(arr)
  of BuildList:
    var arr = newSeq[Value](ins.elements.len)
    for i in 0 ..< arr.len:
      arr[i] = run ins.elements[i]
    result = toValue(arr)
  of BuildSet:
    var arr = initHashSet[Value](ins.elements.len)
    for e in ins.elements:
      arr.incl(run e)
    result = toValue(arr)
  of BuildTable:
    var arr = initTable[Value, Value](ins.entries.len)
    for k, v in ins.entries.items:
      arr[run k] = run v
    result = toValue(arr)
