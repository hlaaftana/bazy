import "."/[primitives, arrays, values, types], std/[sets, tables]

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

proc call*(fun: Function, args: sink Array[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  var newStack = fun.stack.shallowRefresh()
  for i in 0 ..< args.len:
    newStack.set(i, args[i])
  result = run(fun.instruction, newStack, effectHandler)

proc call*(fun: Value, args: sink Array[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  case fun.kind
  of vkNativeFunction:
    result = fun.nativeFunctionValue(args.toOpenArray(0, args.len - 1))
  of vkFunction:
    result = fun.functionValue.call(args, effectHandler)
  else:
    discard # error

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: EffectHandler = nil): Value =
  template run(instr; stack = stack; effectHandler = effectHandler): untyped =
    run(instr, stack, effectHandler)
  let ins = ins[]
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
    result = fn.call(args, effectHandler)
  of Dispatch:
    var args = newArray[Value](ins.dispatchArguments.len)
    for i in 0 ..< args.len:
      args[i] = run ins.dispatchArguments[i]
    for ts, fnInstr in ins.dispatchFunctions.items:
      block accepted:
        for i in 0 ..< args.len:
          if not args[i].checkType(ts[i]):
            break accepted
        let fn = run fnInstr
        result = fn.call(args, effectHandler)
        break
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
        let val = f.call([effect].toArray)
        if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val)):
          return false
        val.toBool
    else:
      discard
    result = run(ins.effectEmitter, stack, handler)
  of BuildTuple:
    if ins.elements.len <= 255:
      var arr = newArray[Value](ins.elements.len)
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
  of AddInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.integerValue + b.integerValue)
  of SubInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.integerValue - b.integerValue)
  of MulInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.integerValue * b.integerValue)
  of DivInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.integerValue div b.integerValue)
  of AddFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.floatValue + b.floatValue)
  of SubFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.floatValue - b.floatValue)
  of MulFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.floatValue * b.floatValue)
  of DivFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.floatValue / b.floatValue)
