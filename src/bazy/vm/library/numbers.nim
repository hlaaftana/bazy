import ".."/[compilation, primitives, values]

import common

import std/macros

macro callOp(op: static string, args: varargs[untyped]): untyped =
  newCall(ident(op), args[0..^1])

import std/math

module numbers:
  define "Int", Ty(Int32)
  define "Float", Ty(Float32)
  define "Uint", Ty(Uint32)
  define "Int64", Ty(Int64)
  define "Float64", Ty(Float64)
  define "Uint64", Ty(Uint64)
  template unarySingle(op: static string, k) =
    fn op, [Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`)
  template unarySingleAlias(name, op: static string, k) =
    fn op, [Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`)
  template unary(op: static string) {.used.} =
    unarySingle op, int32
    unarySingle op, uint32
    unarySingle op, float32
  template binarySingle(op: static string, k) =
    fn op, [Ty(`k`), Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binarySingleAlias(name, op: static string, k) =
    fn name, [Ty(`k`), Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binary(op: static string) =
    binarySingle op, int32
    binarySingle op, uint32
    binarySingle op, float32
  template binarySingleBool(op: static string, k) =
    fn op, [Ty(`k`), Ty(`k`)], Ty(Bool):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binaryBool(op: static string) =
    binarySingleBool op, int32
    binarySingleBool op, uint32
    binarySingleBool op, float32
  unarySingle "+", int32
  unarySingle "+", float32
  unarySingle "-", int32
  unarySingle "-", float32
  binary "+"
  binarySingle "+", uint32
  binary "-"
  binary "*"
  binarySingle "div", int32
  binarySingle "div", uint32
  template floatDivide(k) =
    fn "/", [Ty(`k`), Ty(`k`)], Ty(Float32):
      toValue args[0].`k Value` / args[1].`k Value`
  floatDivide int32
  floatDivide float32
  template instr(name, instructionName, k) =
    typedTempl name, [Ty(`k`), Ty(`k`)], Ty(`k`):
      toValue Statement(kind: skBinaryInstruction,
        binaryInstructionKind: instructionName,
        binary1: args[0].toInstruction,
        binary2: args[1].toInstruction,
        knownType: Ty(`k`))
  instr "+", AddInt, int32
  instr "+", AddFloat, float32
  instr "-", SubInt, int32
  instr "-", SubFloat, float32
  instr "*", MulInt, int32
  instr "*", MulFloat, float32
  instr "div", DivInt, int32
  instr "/", DivFloat, float32
  binary "mod"
  binaryBool "=="
  binaryBool "<"
  binaryBool "<="
  binaryBool ">"
  binaryBool ">="
  unarySingleAlias "!", "not", uint32
  unarySingleAlias "!", "not", int32
  binarySingleAlias "&", "and", uint32
  binarySingleAlias "&", "and", int32
  binarySingleAlias "|", "or", uint32
  binarySingleAlias "|", "or", int32
  binarySingleAlias ">>", "shr", uint32
  binarySingleAlias ">>", "shr", int32
  binarySingleAlias "<<", "shl", uint32
  binarySingleAlias "<<", "shl", int32
  binarySingle "xor", uint32
  binarySingle "xor", int32
  # todo: conversions, hex, binary
  # maybe more instructions
