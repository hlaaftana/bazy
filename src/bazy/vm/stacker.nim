import "."/[primitives, compilation, arrays]

# xxx do some kind of register last use analysis to merge some registers

type
  Register* = distinct int32
  BytePos* = distinct int32

  StackInstructionKind* = enum
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
  
  StackInstruction* = object
    pos*: BytePos
    case kind*: StackInstructionKind
    of NoOp: discard
    of SetRegisterConstant:
      src*: tuple[res: Register, constant: Value] # xxx ???? Value?
    of SetRegisterRegister:
      srr*: tuple[res, val: Register]
    of FunctionCall: discard
    of Dispatch: discard
    of VariableGet: discard
    of VariableSet: discard
    of FromImportedStack: discard
    of SetAddress: discard
    of ArmStack: discard
    of IfJump:
      ifj*: tuple[cond: Register, truePos, falsePos: BytePos]
    of Jump:
      jmp*: tuple[pos: BytePos]
    of EmitEffect: discard
    of HandleEffect: discard
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

  StackContext* = object
    instructions*: seq[StackInstruction]
    registerCount*: int

proc stack*(context: Context, s: Statement, stack: var StackContext): Register =
  # result is register of value
  case s.kind
  of skNone:
    discard # this should push NoValue but whatever
  of skConstant: discard
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

proc stacked*(context: Context, body: Statement): StackContext =
  var stack = StackContext()
  discard stack(context, body, stack)
  result = stack
