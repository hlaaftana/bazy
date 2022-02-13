import "."/[primitives, arrays, values], std/[sets, tables]

type EffectHandler* = proc (effect: Value): bool
  ## returns true to continue execution

template toNegatedBool*(val: Value): bool =
  val.integerValue == 0

template toBool*(val: Value): bool =
  val.integerValue != 0

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: EffectHandler = nil): Value

template run(instr: Instruction, stack, effectHandler): Value =
  let val = evaluate(instr, stack, effectHandler)
  if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val.effectValue[])):
    return val
  val

proc call*(fun: Function, args: sink SafeArray[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  var newStack = fun.stack.shallowRefresh()
  for i in 0 ..< args.len:
    newStack.set(i, args[i])
  result = run(fun.instruction, newStack, effectHandler)

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: EffectHandler = nil): Value =
  template run(instr; stack = stack; effectHandler = effectHandler): untyped =
    run(instr, stack, effectHandler)
  case ins.kind
  of NoOp:
    result = Value(kind: vkNone)
  of Constant:
    result = ins.constantValue
  of FunctionCall:
    let fn = run ins.function
    var args = newSafeArray[Value](ins.arguments.len)
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
    result = stack.get(ins.variableGetIndex)
  of VariableSet:
    result = run ins.variableSetValue
    stack.set(ins.variableSetIndex, result)
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
        let val = f.call([effect].toSafeArray)
        if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val)):
          return false
        val.toBool
    else:
      discard
    result = run(ins.effectEmitter, stack, handler)
  of BuildTuple:
    if ins.elements.len <= 255:
      var arr = newSafeArray[Value](ins.elements.len)
      for i in 0 ..< arr.len:
        arr[i] = run ins.elements[i]
      result = toValue(arr)
    else:
      var arr = newSafeArray[Value](ins.elements.len)
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
