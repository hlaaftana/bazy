import ".."/[compilation, primitives, values]

import common

import std/macros

macro callOp(op: static string, args: varargs[untyped]): untyped =
  newCall(ident(op), args[0..^1])

import std/math

module numbers:
  define "Int", Ty(Integer)
  define "Float", Ty(Float)
  define "Unsigned", Ty(Unsigned)
  template unarySingle(op: static string, k) =
    fn op, [Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`)
  template unary(op: static string) {.used.} =
    unarySingle op, integer
    unarySingle op, unsigned
    unarySingle op, float
  template binarySingle(op: static string, k) =
    fn op, [Ty(`k`), Ty(`k`)], Ty(`k`):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binary(op: static string) =
    binarySingle op, integer
    binarySingle op, unsigned
    binarySingle op, float
  template binarySingleBool(op: static string, k) =
    fn op, [Ty(`k`), Ty(`k`)], Ty(Boolean):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binaryBool(op: static string) =
    binarySingleBool op, integer
    binarySingleBool op, unsigned
    binarySingleBool op, float
  unarySingle "+", integer
  unarySingle "+", float
  unarySingle "-", integer
  unarySingle "-", float
  binary "+"
  binary "-"
  binary "*"
  binarySingle "div", integer
  binarySingle "div", unsigned
  template floatDivide(k) =
    fn "/", [Ty(`k`), Ty(`k`)], Ty(Float):
      toValue args[0].`k Value` / args[1].`k Value`
  floatDivide integer
  floatDivide float
  when false:
    template instr(name, instructionName, k) =
      typedTempl name, [Ty(`k`), Ty(`k`)], Ty(`k`):
        toValue Statement(kind: skBinaryInstruction,
          instructionKind: instructionName,
          binary1: scope.compile(args[0]),
          binary2: scope.compile(args[1]),
          cachedType: Ty(`k`))
    instr "+", AddInt, integer
  binary "mod"
  binaryBool "=="
  # todo: add bools, logic, bitwise, comparison
  # maybe bool not as property yet 
