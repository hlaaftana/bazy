import
  std/[sets, tables],
  ./[primitives, arrays, valueconstr, checktype]

# XXX use linearizer for bytecode and evaluate that without recursion (but maybe keep this)

proc get*(stack: Stack, index: int): lent Value {.inline.} =
  stack.stack[index]
proc set*(stack: Stack, index: int, value: sink Value) {.inline.} =
  stack.stack[index] = value

proc shallowRefresh*(stack: Stack): Stack =
  result = Stack(imports: stack.imports)
  var newStack = newArray[Value](stack.stack.len)
  for i in 0 ..< stack.stack.len:
    newStack[i] = stack.stack[i]
  result.stack = newStack

type EffectHandler* = proc (effect: Value): bool
  ## returns true to continue execution

template toNegatedBool*(val: Value): bool =
  not val.boolValue

template toBool*(val: Value): bool =
  val.boolValue

proc evaluate*(ins: Instruction, stack: Stack, effectHandler: EffectHandler = nil): Value

template run(instr: Instruction, stack, effectHandler): Value =
  let val = evaluate(instr, stack, effectHandler)
  if val.kind == vkEffect and (effectHandler.isNil or not effectHandler(val.effectValue.unref)):
    return val
  val

proc call*(fun: TreeWalkFunction, args: sink Array[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  var newStack = fun.stack.shallowRefresh()
  for i in 0 ..< args.len:
    newStack.set(i, args[i])
  result = run(fun.instruction, newStack, effectHandler)

proc call*(fun: Value, args: sink Array[Value], effectHandler: EffectHandler = nil): Value {.inline.} =
  case fun.kind
  of vkNativeFunction:
    result = fun.boxedValue.nativeFunctionValue(args.toOpenArray(0, args.len - 1))
  of vkFunction:
    result = fun.boxedValue.functionValue.call(args, effectHandler)
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
    block dispatch:
      for ts, fnInstr in ins.dispatchFunctions.items:
        block accepted:
          for i in 0 ..< args.len:
            if not args[i].checkType(ts[i]):
              break accepted
          let fn = run fnInstr
          result = fn.call(args, effectHandler)
          break dispatch
  of Sequence:
    for instr in ins.sequence:
      result = run instr
  of VariableGet:
    result = stack.get(ins.variableGetIndex)
  of VariableSet:
    result = run ins.variableSetValue
    stack.set(ins.variableSetIndex, result)
  of GetAddress:
    var s = stack
    var i = ins.getAddress.len
    while i > 1:
      dec i
      s = s.imports[ins.getAddress[i]]
    result = s.get(ins.getAddress[0])
  of SetAddress:
    result = run ins.setAddressValue
    var s = stack
    var i = ins.setAddress.len
    while i > 1:
      dec i
      s = s.imports[ins.setAddress[i]]
    s.set(ins.setAddress[0], result)
  of ArmStack:
    result = run ins.armStackFunction
    result.boxedValue.functionValue.stack.imports[0] = stack
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
    result.effectValue.store(run ins.effect)
  of HandleEffect:
    let h = run ins.effectHandler
    var handler: proc (effect: Value): bool
    case h.kind
    of vkNativeFunction:
      let f = h.boxedValue.nativeFunctionValue
      handler = proc (effect: Value): bool =
        f([effect]).toBool
    of vkFunction:
      let f = h.boxedValue.functionValue
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
  of GetIndex:
    let x = run ins.getIndexAddress
    case x.kind
    of vkList:
      result = x.boxedValue.listValue.unref[ins.getIndex]
    of vkArray:
      result = x.boxedValue.tupleValue.unref[ins.getIndex]
    of vkString:
      result = toValue(x.boxedValue.stringValue.unref[ins.getIndex].int)
    else: discard # error
  of SetIndex:
    let x = run ins.setIndexAddress
    result = run ins.setIndexValue
    case x.kind
    of vkList:
      x.boxedValue.listValue.unref[ins.setIndex] = result
    of vkArray:
      x.boxedValue.tupleValue.unref[ins.setIndex] = result
    of vkString:
      assert result.kind == vkInt32 and result.int32Value >= 0 and result.int32Value <= 255
      x.boxedValue.stringValue.unref[ins.setIndex] = result.int32Value.char
    else: discard # error
  of AddInt:
    # XXX (4) account for boxing probably best with vkBoxedInt32
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.int32Value + b.int32Value)
  of SubInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.int32Value - b.int32Value)
  of MulInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.int32Value * b.int32Value)
  of DivInt:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.int32Value div b.int32Value)
  of AddFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.float32Value + b.float32Value)
  of SubFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.float32Value - b.float32Value)
  of MulFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.float32Value * b.float32Value)
  of DivFloat:
    let a = run ins.binary1
    let b = run ins.binary2
    result = toValue(a.float32Value / b.float32Value)
  of NegInt:
    let a = run ins.unary
    result = toValue(-a.int32Value)
  of NegFloat:
    let a = run ins.unary
    result = toValue(-a.float32Value)
