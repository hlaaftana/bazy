import "."/[primitives, compilation, arrays]

# xxx do some kind of register last use analysis to merge some registers
# xxx constant pool can be long string of serialized values,
# then they are deserialized and cached into registers when loaded

type
  Register* = distinct uint16 # 32 bit would be nice
    # xxx maybe unify with constants, make signed, index constant pool when negative
  BytePos* = distinct uint16

  LinearInstructionKind* = enum
    NoOp
    SetRegisterConstant # push
    SetRegisterRegister # mov
    NullaryCall
    UnaryCall
    BinaryCall
    TernaryCall
    TupleCall
    Dispatch
    VariableGet # go
    VariableSet # go
    GetAddress # go
    SetAddress # go
    ArmStack # stay but arm constants
    IfJump
    Jump
    # effect, can emulate goto
    EmitEffect
    PushEffectHandler
    PopEffectHandler
    # collection
    InitTuple
    InitTupleConstSize
    GetIndex
    SetIndex
    GetConstIndex
    SetConstIndex
    # binary
    AddInt32, SubInt32, MulInt32, DivInt32
    AddFloat32, SubFloat32, MulFloat32, DivFloat32
    # unary
    NegInt32, NegFloat32
  
  LinearInstruction* = object
    # cannot recurse
    pos*: BytePos
    case kind*: LinearInstructionKind
    of NoOp: discard
    of SetRegisterConstant:
      src*: tuple[res: Register, constant: Value]
        # xxx should be an index in constant pool instead
        # reference constants also have special behavior when loaded
    of SetRegisterRegister:
      srr*: tuple[res, val: Register]
    of NullaryCall:
      ncall*: tuple[res, callee: Register]
    of UnaryCall:
      ucall*: tuple[res, callee, arg1: Register]
    of BinaryCall:
      bcall*: tuple[res, callee, arg1, arg2: Register]
    of TernaryCall:
      tcall*: tuple[res, callee, arg1, arg2, arg3: Register]
    of TupleCall:
      tupcall*: tuple[res, callee, args: Register]
    of Dispatch:
      dispatch*: tuple[]
    of VariableGet:
      vg*: tuple[res: Register, index: int32]
    of VariableSet:
      vs*: tuple[index: int32, val: Register]
    of GetAddress:
      gadr*: tuple[res: Register, address: Array[int32]]
    of SetAddress:
      sadr*: tuple[address: Array[int32], val: Register]
    of ArmStack:
      arm*: tuple[fun: Register]
    of IfJump:
      ifj*: tuple[cond: Register, truePos, falsePos: BytePos]
    of Jump:
      jmp*: tuple[pos: BytePos]
    of EmitEffect:
      emit*: tuple[]
    of PushEffectHandler:
      pueh*: tuple[]
    of PopEffectHandler:
      poeh*: tuple[]
    of InitTuple:
      itup*: tuple[res, siz: Register]
    of InitTupleConstSize:
      itcs*: tuple[res: Register, siz: int32]
    of GetConstIndex:
      gci*: tuple[res, coll: Register, ind: int32]
    of SetConstIndex:
      sci*: tuple[coll: Register, ind: int32, val: Register]
    of GetIndex:
      gri*: tuple[res, coll, ind: Register]
    of SetIndex:
      sri*: tuple[coll, ind, val: Register]
    of AddInt32, SubInt32, MulInt32, DivInt32,
      AddFloat32, SubFloat32, MulFloat32, DivFloat32:
      binary*: tuple[res, arg1, arg2: Register]
    of NegInt32, NegFloat32:
      unary*: tuple[res, arg: Register]
  
  SpecialRegister = enum
    Void

  LinearFunction* = ref object
    instructions*: seq[LinearInstruction]
    byteCount*: int
    registerCount*: int
    usedSpecialRegisters: set[SpecialRegister]
    specialRegisters: array[SpecialRegister, Register]
  
  ResultKind = enum
    Value
    Statement
    SetRegister
  
  Result = object
    case kind: ResultKind
    of Value:
      value: Register
    of Statement: discard
    of SetRegister:
      register: Register

const
  lowBinary = AddInt32
  highBinary = DivFloat32
  lowUnary = NegInt32
  highUnary = NegFloat32
  instructionKindMap: array[low(BinaryInstructionKind) .. high(UnaryInstructionKind), LinearInstructionKind] = [
    # binary
    AddInt: AddInt32, SubInt: SubInt32, MulInt: MulInt32, DivInt: DivInt32,
    AddFloat: AddFloat32, SubFloat: SubFloat32, MulFloat: MulFloat32, DivFloat: DivFloat32,
    # unary
    NegInt: NegInt32, NegFloat: NegFloat32
  ]

proc byteCount*(atom: Register | int32 | BytePos): int =
  result = sizeof(atom)

proc byteCount*[T: tuple](tup: T): int =
  for a in fields(tup):
    result += sizeof(a)

proc byteCount*(atom: Array[int32]): int =
  result = sizeof(int32(atom.len)) + atom.len * sizeof(int32)

proc byteCount*(instr: LinearInstruction): int =
  result = sizeof(instr.kind)
  for a in instr.fields:
    when a is tuple:
      result += byteCount(a)

proc addBytes*(bytes: var openarray[byte], i: var int, instr: LinearInstruction) =
  template add(b: byte) =
    bytes[i] = b
    inc i
  template add(r: Register | BytePos) =
    let u = r.uint16
    add(byte((u shr 8) and 0xFF))
    add(byte(u and 0xFF))
  template add(ii: int32) =
    let u = cast[uint32](ii)
    add(byte((u shr 24) and 0xFF))
    add(byte((u shr 16) and 0xFF))
    add(byte((u shr 0) and 0xFF))
    add(byte(u and 0xFF))
  template add(ia: Array[int32]) =
    add(int32(ia.len))
    for x in ia:
      add(x)
  template add(tup: tuple) =
    for a in tup.fields:
      add(a)
  add instr.kind.byte
  case instr.kind
  of NoOp: discard
  of SetRegisterConstant:
    discard "add instr.src" # XXX serialize values (but not here, in constant pool)
  of SetRegisterRegister:
    add instr.srr
  of NullaryCall:
    add instr.ncall
  of UnaryCall:
    add instr.ucall
  of BinaryCall:
    add instr.bcall
  of TernaryCall:
    add instr.tcall
  of TupleCall:
    add instr.tupcall
  of Dispatch:
    add instr.dispatch
  of VariableGet:
    add instr.vg
  of VariableSet:
    add instr.vs
  of GetAddress:
    add instr.gadr
  of SetAddress:
    add instr.sadr
  of ArmStack:
    add instr.arm
  of IfJump:
    add instr.ifj
  of Jump:
    add instr.jmp
  of EmitEffect:
    add instr.emit
  of PushEffectHandler:
    add instr.pueh
  of PopEffectHandler:
    add instr.poeh
  of InitTuple:
    add instr.itup
  of InitTupleConstSize:
    add instr.itcs
  of GetConstIndex:
    add instr.gci
  of SetConstIndex:
    add instr.sci
  of GetIndex:
    add instr.gri
  of SetIndex:
    add instr.sri
  of AddInt32, SubInt32, MulInt32, DivInt32,
    AddFloat32, SubFloat32, MulFloat32, DivFloat32:
    add instr.binary
  of NegInt32, NegFloat32:
    add instr.unary

proc add(fn: LinearFunction, instr: LinearInstruction) =
  var instr = instr
  instr.pos = fn.byteCount.BytePos
  fn.instructions.add(instr)
  fn.byteCount += instr.byteCount

# xxx add way to read instruction from bytes

proc newRegister(fn: LinearFunction): Register =
  result = fn.registerCount.Register
  inc fn.registerCount

proc getRegister(fn: LinearFunction, sr: SpecialRegister): Register =
  if sr in fn.usedSpecialRegisters:
    result = fn.specialRegisters[sr]
  else:
    result = fn.newRegister()
    fn.specialRegisters[sr] = result
    fn.usedSpecialRegisters.incl(sr)

proc linearize*(context: Context, fn: LinearFunction, result: var Result, s: Statement) =
  type Instr = LinearInstruction
  template value(s: Statement): Register =
    var res = Result(kind: Value)
    linearize(context, fn, res, s)
    res.value
  var statementResult = Result(kind: Statement)
  template statement(s: Statement) =
    linearize(context, fn, statementResult, s)
  let resultKind = result.kind
  case s.kind
  of skNone:
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: SetRegisterConstant, src: (res: result.register, constant: Value(kind: vkNone))))
    of Value:
      result.value = fn.newRegister()
      fn.add(Instr(kind: SetRegisterConstant, src: (res: result.value, constant: Value(kind: vkNone))))
    of Statement: discard # nothing
  of skConstant:
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: SetRegisterConstant, src: (res: result.register, constant: s.constant)))
    of Value, Statement:
      let reg = fn.newRegister()
      fn.add(Instr(kind: SetRegisterConstant, src: (res: reg, constant: s.constant)))
      if result.kind == Value:
        result.value = reg
  of skFunctionCall:
    let f = value(s.callee)
    let res =
      case resultKind
      of SetRegister: result.register
      of Value:
        result.value = fn.newRegister()
        result.value
      of Statement: fn.getRegister(Void)
    case s.arguments.len
    of 0:
      fn.add(Instr(kind: NullaryCall, ncall: (res: res, callee: f)))
    of 1:
      fn.add(Instr(kind: UnaryCall, ucall: (res: res, callee: f, arg1: value(s.arguments[0]))))
    of 2:
      fn.add(Instr(kind: BinaryCall, bcall: (res: res, callee: f, arg1: value(s.arguments[0]), arg2: value(s.arguments[1]))))
    of 3:
      fn.add(Instr(kind: TernaryCall, tcall: (res: res, callee: f, arg1: value(s.arguments[0]), arg2: value(s.arguments[1]), arg3: value(s.arguments[2]))))
    else:
      let args = fn.newRegister()
      fn.add(Instr(kind: InitTupleConstSize, itcs: (res: args, siz: s.arguments.len.int32)))
      for i, a in s.arguments:
        fn.add(Instr(kind: SetConstIndex, sci: (coll: args, ind: i.int32, val: value(a))))
      fn.add(Instr(kind: TupleCall, tupcall: (res: res, callee: f, args: args)))
  of skDispatch: discard
  of skSequence:
    let h = s.sequence.len - 1
    for i in 0 ..< h:
      statement(s.sequence[i])
    linearize(context, fn, result, s.sequence[h])
  of skVariableGet:
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: VariableGet, vg: (res: result.register, index: s.variableGetIndex.int32)))
    of Value, Statement:
      let reg = fn.newRegister()
      fn.add(Instr(kind: VariableGet, vg: (res: reg, index: s.variableGetIndex.int32)))
      if result.kind == Value:
        result.value = reg
  of skVariableSet:
    let val = value(s.variableSetValue)
    fn.add(Instr(kind: VariableSet, vs: (index: s.variableSetIndex.int32, val: val)))
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: SetRegisterRegister, srr: (res: result.register, val: val)))
    of Value:
      result.value = val
    of Statement: discard
  of skGetAddress:
    var arr = newArray[int32](s.getAddress.indices.len)
    for i, x in s.getAddress.indices: arr[i] = x.int32
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: GetAddress, gadr: (res: result.register, address: arr)))
    of Value, Statement:
      let reg = fn.newRegister()
      fn.add(Instr(kind: GetAddress, gadr: (res: reg, address: arr)))
      if result.kind == Value:
        result.value = reg
  of skSetAddress:
    var arr = newArray[int32](s.setAddress.indices.len)
    for i, x in s.setAddress.indices: arr[i] = x.int32
    let val = value(s.setAddressValue)
    fn.add(Instr(kind: SetAddress, sadr: (address: arr, val: val)))
    case resultKind
    of SetRegister:
      fn.add(Instr(kind: SetRegisterRegister, srr: (res: result.register, val: val)))
    of Value:
      result.value = val
    of Statement: discard
  of skArmStack:
    fn.add(Instr(kind: ArmStack, arm: (fun: value(s.armStackFunction))))
  of skIf: discard
  of skWhile: discard
  of skDoUntil: discard
  of skEmitEffect: discard
  of skHandleEffect: discard
  of skTuple: discard
  of skList: discard
  of skSet: discard
  of skTable: discard
  of skGetIndex: discard
  of skSetIndex: discard
  of skUnaryInstruction:
    let res =
      case resultKind
      of SetRegister: result.register
      of Value:
        result.value = fn.newRegister()
        result.value
      of Statement: fn.getRegister(Void)
    fn.add(Instr(kind: range[lowUnary..highUnary] instructionKindMap[s.unaryInstructionKind], unary: (res: res, arg: value(s.unary))))
  of skBinaryInstruction:
    let res =
      case resultKind
      of SetRegister: result.register
      of Value:
        result.value = fn.newRegister()
        result.value
      of Statement: fn.getRegister(Void)
    fn.add(Instr(kind: range[lowBinary..highBinary] instructionKindMap[s.binaryInstructionKind], binary: (res: res, arg1: value(s.binary1), arg2: value(s.binary2))))

proc linear*(context: Context, body: Statement): LinearFunction =
  var stack = LinearFunction()
  var res = Result(kind: Value)
  linearize(context, stack, res, body)
  result = stack
