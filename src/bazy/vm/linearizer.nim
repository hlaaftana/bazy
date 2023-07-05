import "."/[primitives, compilation, arrays]

# xxx do some kind of register last use analysis to merge some registers

type
  Register* = distinct uint16
  BytePos* = distinct uint16

  LinearInstructionKind* = enum
    NoOp
    SetRegisterConstant # push
    SetRegisterRegister # mov
    FunctionCall
    Dispatch
    VariableGet
    VariableSet
    FromImportedStack
    SetAddress
    ArmStack
    IfJump
    Jump
    # effect, can emulate goto
    EmitEffect
    HandleEffect
    # collection
    #BuildTuple BuildList BuildSet BuildTable
    GetConstantIndex
    SetConstantIndex
    GetRegisterIndex
    SetRegisterIndex
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
    of FunctionCall:
      call*: tuple[callee: Register, ]
    of Dispatch:
      dispatch*: tuple[]
    of VariableGet:
      vg*: tuple[]
    of VariableSet:
      vs*: tuple[]
    of FromImportedStack:
      fis*: tuple[]
    of SetAddress:
      sadr*: tuple[]
    of ArmStack:
      arm*: tuple[]
    of IfJump:
      ifj*: tuple[cond: Register, truePos, falsePos: BytePos]
    of Jump:
      jmp*: tuple[pos: BytePos]
    of EmitEffect:
      emit*: tuple[]
    of HandleEffect:
      handle*: tuple[]
    of GetConstantIndex:
      gci*: tuple[res, coll: Register, ind: int32]
    of SetConstantIndex:
      sci*: tuple[coll: Register, ind: int32, val: Register]
    of GetRegisterIndex:
      gri*: tuple[res, coll, ind: Register]
    of SetRegisterIndex:
      sri*: tuple[coll, ind, val: Register]
    of AddInt32, SubInt32, MulInt32, DivInt32,
      AddFloat32, SubFloat32, MulFloat32, DivFloat32:
      binary*: tuple[res, arg1, arg2: Register]
    of NegInt32, NegFloat32:
      unary*: tuple[res, arg: Register]

  LinearFunction* = ref object
    instructions*: seq[LinearInstruction]
    byteCount*: int
    registerCount*: int
  
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

const instructionKindMap: array[low(BinaryInstructionKind) .. high(UnaryInstructionKind), LinearInstructionKind] = [
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
  template add(tup: tuple) =
    for a in tup.fields:
      add(a)
  add instr.kind.byte
  case instr.kind
  of NoOp: discard
  of SetRegisterConstant:
    discard "add instr.src" # XXX serialize values
  of SetRegisterRegister:
    add instr.srr
  of FunctionCall:
    add instr.call
  of Dispatch:
    add instr.dispatch
  of VariableGet:
    add instr.vg
  of VariableSet:
    add instr.vs
  of FromImportedStack:
    add instr.fis
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
  of HandleEffect:
    add instr.handle
  of GetConstantIndex:
    add instr.gci
  of SetConstantIndex:
    add instr.sci
  of GetRegisterIndex:
    add instr.gri
  of SetRegisterIndex:
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

proc newRegister(fn: LinearFunction): Register =
  result = fn.registerCount.Register
  inc fn.registerCount

# xxx maybe special registers for specific behaviors
# i.e. a single register to load constants into

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
  of skFunctionCall: discard
  of skDispatch: discard
  of skSequence: discard
  of skVariableGet: discard
  of skVariableSet: discard
  of skFromImportedStack: discard
  of skSetAddress: discard
  of skArmStack: discard
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
  of skUnaryInstruction: discard
  of skBinaryInstruction: discard

proc linear*(context: Context, body: Statement): LinearFunction =
  var stack = LinearFunction()
  var res = Result(kind: Value)
  linearize(context, stack, res, body)
  result = stack
