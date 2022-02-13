import ".."/[compilation, primitives, values]

import common

import std/macros

macro callOp(op: static string, args: varargs[untyped]): untyped =
  newCall(ident(op), args[0..^1])

import std/math

module numbers:
  template unarySingle(op: static string, k) =
    fn op, [makeType(`k`)], makeType(`k`):
      toValue callOp(`op`, args[0].`k Value`)
  template unary(op: static string) =
    unarySingle op, integer
    unarySingle op, unsigned
    unarySingle op, float
  template binarySingle(op: static string, k) =
    fn op, [makeType(`k`), makeType(`k`)], makeType(`k`):
      toValue callOp(`op`, args[0].`k Value`, args[1].`k Value`)
  template binary(op: static string) =
    binarySingle op, integer
    binarySingle op, unsigned
    binarySingle op, float
  unarySingle "+", integer
  unarySingle "+", float
  unarySingle "-", integer
  unarySingle "-", float
  binary "+"
  binary "-"
  binary "*"
  template floatDivide(k) =
    fn "/", [makeType(`k`), makeType(`k`)], makeType(Float):
      toValue args[0].`k Value` / args[1].`k Value`
  floatDivide integer
  floatDivide float
  binarySingle "div", integer
  binarySingle "div", unsigned
  binary "mod"
  # todo: add bools, logic, bitwise, comparison
  # maybe bool not as property yet 
