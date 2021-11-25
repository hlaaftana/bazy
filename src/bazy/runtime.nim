import types, tables

type
  ValueKind* = enum
    vkNone # some kind of null value
    vkInteger, vkFloat # not sure of size
    vkFunction # argument should be tuple?
    vkTuple # VLA, not necessarily heterogenously typed
    vkReference # reference to value
    vkString, vkSeq # references to string and seq of value (string is general byte seq)
    vkComposite # like tuple, but fields are tied to names and unordered
    vkNominalTyped # value with an attached nominal type, unfortunately this is pointer to save memory

  NominalTypedValue* = object
    nominalType*: Type
    value*: Value

  Value* {.acyclic.} = object
    case kind*: ValueKind
    of vkNone: discard
    of vkInteger:
      integerValue*: int
    of vkFloat:
      floatValue*: float
    of vkFunction:
      functionValue*: proc (args: sink seq[Value]): Value
    of vkTuple:
      # supposed to be just length and pointer, might do 16 bits length 48 bits pointer
      tupleValue*: ref seq[Value]
    of vkReference:
      referenceValue*: ref Value
    of vkString:
      stringValue*: ref string
    of vkSeq:
      seqValue*: ref seq[Value]
    of vkComposite:
      # supposed to be represented more efficiently
      compositeValue*: ref Table[string, Value]
    of vkNominalTyped:
      nominalValue*: ref NominalTypedValue
