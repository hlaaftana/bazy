# bytecode interpreter

import ./[primitives, linearizer, arrays, treewalk, guesstype, checktype, typebasics, valueconstr],
  std/[sets, tables]

proc run*(lf: LinearFunction): Value =
  var registers = newArray[Value](lf.registerCount)
  template put(reg: Register, val: Value) =
    registers[reg.int32] = val
  template get(reg: Register): Value =
    registers[reg.int32]
  template mov(src, dest: Register) =
    registers[dest.int32] = registers[src.int32]

  var
    effectHandlers: seq[EffectHandler]
    effectHandler: EffectHandler

  let instructions {.cursor.} = lf.instructions
  var i = 0
  template read(b: var byte) =
    assert i < instructions.len
    b = instructions[i]
    inc i
  template read(b: var uint16) =
    assert i + 1 < instructions.len
    b = cast[ptr uint16](addr instructions[i])[]
    inc i, 2
  template read(b: var uint32) =
    assert i + 3 < instructions.len
    b = cast[ptr uint32](addr instructions[i])[]
    inc i, 4
  template read(r: var (Register | JumpLocation | Constant)) =
    var u: uint16
    read(u)
    r = typeof(r)(u)
  template read(ii: var int32) =
    var u: uint32
    read(u)
    ii = cast[int32](u)
  template read(ia: var Array[int32]) =
    var len: int32
    read(len)
    ia = newArray[int32](len)
    for x in ia.mitems:
      read(x)
  template read[T: tuple](tup: var T) =
    for a in tup.fields:
      read(a)
  
  template jump(p: JumpLocation) =
    i = p.int

  template checkEffect(v: Value): Value =
    let val = v
    if val.kind == vkEffect:
      let eff = val.effectValue.unref
      block handle:
        for i in countdown(effectHandlers.high, 0):
          if effectHandlers[i](eff):
            break handle
        return val
    val

  while i < instructions.len:
    var opcode: byte
    read(opcode)
    {.computedGoto.}
    var instr = LinearInstruction(kind: LinearInstructionKind(opcode))
    case instr.kind
    of NoOp: discard
    of LoadConstant:
      read instr.lc
      # XXX shallow copy here (not really needed but still)
      put instr.lc.res, lf.constants[instr.lc.constant.int32]
    of SetRegisterRegister:
      read instr.srr
      mov instr.srr.res, instr.srr.val
    of NullaryCall:
      read instr.ncall
      let fn {.cursor.} = get instr.ncall.callee
      case fn.kind
      of vkNativeFunction:
        put instr.ncall.res, checkEffect fn.boxedValue.nativeFunctionValue([])
      of vkFunction:
        put instr.ncall.res, checkEffect fn.boxedValue.functionValue.call(
          default(Array[Value]),
          effectHandler)
      # XXX (1) handle linear function
      else:
        discard # error
    of UnaryCall:
      read instr.ucall
      let fn {.cursor.} = get instr.ucall.callee
      case fn.kind
      of vkNativeFunction:
        put instr.ucall.res, checkEffect fn.boxedValue.nativeFunctionValue(
          [get instr.ucall.arg1])
      of vkFunction:
        put instr.ucall.res, checkEffect fn.boxedValue.functionValue.call(
          toArray[Value]([get instr.ucall.arg1]),
          effectHandler)
      # XXX (1) handle linear function
      else:
        discard # error
    of BinaryCall:
      read instr.bcall
      let fn {.cursor.} = get instr.bcall.callee
      case fn.kind
      of vkNativeFunction:
        put instr.bcall.res, checkEffect fn.boxedValue.nativeFunctionValue(
          [get instr.bcall.arg1, get instr.bcall.arg2])
      of vkFunction:
        put instr.bcall.res, checkEffect fn.boxedValue.functionValue.call(
          toArray[Value]([get instr.bcall.arg1, get instr.bcall.arg2]),
          effectHandler)
      # XXX (1) handle linear function
      else:
        discard # error
    of TernaryCall:
      read instr.tcall
      let fn {.cursor.} = get instr.tcall.callee
      case fn.kind
      of vkNativeFunction:
        put instr.tcall.res, checkEffect fn.boxedValue.nativeFunctionValue(
          [get instr.tcall.arg1, get instr.tcall.arg2, get instr.tcall.arg3])
      of vkFunction:
        put instr.tcall.res, checkEffect fn.boxedValue.functionValue.call(
          toArray[Value]([get instr.tcall.arg1, get instr.tcall.arg2, get instr.tcall.arg3]),
          effectHandler)
      # XXX (1) handle linear function
      else:
        discard # error
    of TupleCall:
      read instr.tupcall
      let fn {.cursor.} = get instr.tupcall.callee
      let args = (get instr.tupcall.args).tupleValue.unref # maybe move
      case fn.kind
      of vkNativeFunction:
        put instr.tcall.res, checkEffect fn.boxedValue.nativeFunctionValue(
          args.toOpenArray(0, args.len - 1))
      of vkFunction:
        put instr.tcall.res, checkEffect fn.boxedValue.functionValue.call(
          args, effectHandler)
      # XXX (1) handle linear function
      else:
        discard # error
    of TryDispatch:
      read instr.tdisp
      let fn {.cursor.} = get instr.tdisp.callee
      let argsVal = get instr.tdisp.args
      let t = fn.getType
      assert t.kind == tyCompound and t.base.nativeType == ntyFunction
      let argt = t.baseArguments[0]
      if argsVal.checkType(argt):
        let args = argsVal.tupleValue.unref
        case fn.kind
        of vkNativeFunction:
          put instr.tdisp.res, checkEffect fn.boxedValue.nativeFunctionValue(
            args.toOpenArray(0, args.len - 1))
        of vkFunction:
          put instr.tdisp.res, checkEffect fn.boxedValue.functionValue.call(
            args, effectHandler)
        # XXX (1) handle linear function
        else:
          discard # error
        jump instr.tdisp.successPos
    of ArmType:
      read instr.armt
      let tVal = get(instr.armt.typ)
      assert tVal.kind == vkType
      let t = tVal.boxedValue.typeValue
      let val = get(instr.armt.val)
      if val.kind in boxedValueKinds:
        let box = val.boxedValue
        if box.type.isNil:
          new(box.type)
        box.type[] = t
      else:
        var box = toFullValueObj(val)
        new(box.type)
        box.type[] = t
        var newVal = Value(kind: vkBoxed)
        new(newVal.boxedValue)
        newVal.boxedValue[] = box
        put(instr.armt.val, newVal)
    of ArmStack:
      read instr.arm
      let fn = get(instr.arm.fun)
      case fn.kind
      of vkFunction:
        fn.boxedValue.functionValue.stack.set(instr.arm.ind, get(instr.arm.val))
      # XXX (1) handle linear function
      else: discard # error
    of RefreshStack:
      read instr.rfs
      let fn = get(instr.rfs.fun)
      case fn.kind
      of vkFunction:
        fn.boxedValue.functionValue.stack = fn.boxedValue.functionValue.stack.shallowRefresh()
      # XXX (1) handle linear function
      else: discard # error
    of JumpPoint:
      read instr.jpt
      assert i == lf.jumpLocations[instr.jpt.loc.int] + byteCount(instr.jpt)
    of IfTrueJump:
      read instr.iftj
      let cond = get(instr.iftj.cond)
      assert cond.kind == vkBool
      if cond.boolValue:
        jump instr.iftj.truePos
    of IfFalseJump:
      read instr.iffj
      let cond = get(instr.iffj.cond)
      assert cond.kind == vkBool
      if not cond.boolValue:
        jump instr.iffj.falsePos
    of Jump:
      read instr.jmp
      jump instr.jpt.loc
    of EmitEffect:
      read instr.emit
      var eff = Value(kind: vkEffect)
      eff.effectValue.store get(instr.emit.effect)
      discard checkEffect eff
    of PushEffectHandler:
      read instr.pueh
      let h = get instr.pueh.handler
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
      # XXX (1) handle linear function
      else: discard # error
      effectHandlers.add(handler)
    of PopEffectHandler:
      read instr.poeh
      effectHandlers.setLen(effectHandlers.len - 1)
    of InitTuple:
      read instr.coll
      put instr.coll.res, toValue newArray[Value](instr.coll.siz)
    of InitList:
      read instr.coll
      put instr.coll.res, toValue newSeq[Value](instr.coll.siz)
    of InitSet:
      read instr.coll
      put instr.coll.res, toValue initHashSet[Value](instr.coll.siz)
    of InitTable:
      read instr.coll
      put instr.coll.res, toValue initTable[Value, Value](instr.coll.siz)
    of GetConstIndex:
      read instr.gci
      put instr.gci.res, get(instr.gci.coll).tupleValue.unref[instr.gci.ind.int]
    of SetConstIndex:
      read instr.sci
      get(instr.sci.coll).tupleValue[instr.sci.ind.int] = get(instr.sci.val)
    of GetIndex:
      read instr.gri
      let coll = get instr.gri.coll
      let ind = get instr.gri.ind
      # XXX (6) maybe prevent dispatch here
      case coll.kind
      of vkList:
        put instr.gri.res, coll.boxedValue.listValue.unref[ind.unboxStripType.int32Value]
      of vkArray:
        put instr.gri.res, coll.tupleValue.unref[ind.unboxStripType.int32Value]
      of vkString:
        put instr.gri.res, toValue(coll.boxedValue.stringValue.unref[ind.unboxStripType.int32Value].int)
      of vkTable:
        put instr.gri.res, coll.boxedValue.tableValue[ind]
      else: discard # error
    of SetIndex:
      read instr.sri
      let coll = get instr.sri.coll
      let ind = get instr.sri.ind
      let val = get instr.sri.val
      # XXX (6) maybe prevent dispatch here
      case coll.kind
      of vkList:
        coll.boxedValue.listValue.unref[ind.unboxStripType.int32Value] = val
      of vkArray:
        coll.tupleValue[ind.unboxStripType.int32Value] = val
      of vkString:
        coll.boxedValue.stringValue.unref[ind.unboxStripType.int32Value] = val.int32Value.char
      of vkTable:
        coll.boxedValue.tableValue[ind] = val
      of vkSet:
        coll.boxedValue.setValue.incl(val)
      else: discard # error
    of AddInt32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.int32Value
      let b = get(instr.binary.arg2).unboxStripType.int32Value
      put instr.binary.res, toValue(a + b)
    of SubInt32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.int32Value
      let b = get(instr.binary.arg2).unboxStripType.int32Value
      put instr.binary.res, toValue(a - b)
    of MulInt32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.int32Value
      let b = get(instr.binary.arg2).unboxStripType.int32Value
      put instr.binary.res, toValue(a * b)
    of DivInt32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.int32Value
      let b = get(instr.binary.arg2).unboxStripType.int32Value
      put instr.binary.res, toValue(a / b)
    of AddFloat32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.float32Value
      let b = get(instr.binary.arg2).unboxStripType.float32Value
      put instr.binary.res, toValue(a + b)
    of SubFloat32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.float32Value
      let b = get(instr.binary.arg2).unboxStripType.float32Value
      put instr.binary.res, toValue(a - b)
    of MulFloat32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.float32Value
      let b = get(instr.binary.arg2).unboxStripType.float32Value
      put instr.binary.res, toValue(a * b)
    of DivFloat32:
      read instr.binary
      let a = get(instr.binary.arg1).unboxStripType.float32Value
      let b = get(instr.binary.arg2).unboxStripType.float32Value
      put instr.binary.res, toValue(a / b)
    of NegInt32:
      read instr.unary
      let a = get(instr.unary.arg).unboxStripType.int32Value
      put instr.unary.res, toValue(-a)
    of NegFloat32:
      read instr.unary
      let a = get(instr.unary.arg).unboxStripType.float32Value
      put instr.unary.res, toValue(-a)
