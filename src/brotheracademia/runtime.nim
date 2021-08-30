type
  TypeId* = distinct int

  ValueKind* = enum
    vkInteger, vkFloat, vkFunction, vkPointer, vkOther
  
  OtherValueKind* = enum
    ovkString, ovkSeqValue, ovkTyped
  
  OtherValue* = object
    case kind*: OtherValueKind
    of ovkString:
      stringValue*: string
    of ovkSeqValue:
      seqValue*: seq[Value]
    of ovkTyped:
      typeId*: TypeId
      typedData*: pointer

  Value* = object
    case kind*: ValueKind
    of vkInteger:
      integerValue*: int
    of vkFloat:
      floatValue*: float
    of vkFunction:
      functionValue*: proc (args: sink seq[Value])
    of vkPointer:
      pointerValue*: pointer
    of vkOther:
      otherValue*: ref OtherValue

# probably wont do bytecode and instead do expressions with value error tuple
type
  # stack is a seq
  InstructionKind* = enum # most abstract instruction set maybe
    ikStorePointer # stores a value in the stack to a pointer in the stack
    ikReadPointer # reads a value in a pointer to the given index in the stack
    ikStackSetConstant
    ikStackSet
    ikStackAdd
    ikStackLen
    ikStackSetLen
    ikFunctionCall
    ikGoto
    ikCondGoto
  
  Instruction* = object
    case kind*: InstructionKind
    of ikStorePointer:
      spPointerIndex*: int
      spIndex*: int
    of ikReadPointer:
      rpPointerIndex*: int
      rpIndex*: int
    of ikStackSetConstant:
      sscIndex*: int
      sscConstant*: Value # not other
    of ikStackSet:
      ssIndex*: int
      ssValueIndex*: int
    of ikStackAdd:
      saValueIndex*: int
    of ikStackLen:
      slIndex*: int
    of ikStackSetLen:
      sslValueIndex*: int
    of ikFunctionCall:
      fcArgsIndexes*: seq[int]
    of ikGoto:
      gtNumber*: int
    of ikCondGoto:
      cgtIndex*: int
      cgtNumber*: int

