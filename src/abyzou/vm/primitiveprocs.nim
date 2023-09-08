import
  std/[hashes, tables, sets],
  skinsuit/equals,
  ./[primitives, arrays, ids]

template mix(x) =
  mixin hash
  result = result !& hash(x)

template idObject[T: ref](t: type T) {.dirty.} =
  proc hash*(p: t): Hash {.noSideEffect.} =
    if not p.isNil:
      mix p.id
    else:
      mix pointer(nil)
    result = !$ result

  proc `==`*(a, b: t): bool = a.isNil and b.isNil or (not a.isNil and not b.isNil and a.id == b.id)

idObject(TypeBase)
idObject(TypeParameter)

proc hash*(v: FullValueObj): Hash {.noSideEffect.}
proc hash*(v: Value): Hash {.noSideEffect.}
proc hash*(v: Type): Hash {.noSideEffect.}
proc hash*(v: InstructionObj): Hash {.noSideEffect.}

template hashRefObj(T): untyped {.dirty.} =
  proc hash*(v: T): Hash {.noSideEffect.} =
    if v.isNil:
      mix 0
    else:
      mix v[]
    result = !$ result

hashRefObj(ref Value)
hashRefObj(ref Type)
hashRefObj Instruction
hashRefObj Stack

template hashObj(T): untyped {.dirty.} =
  proc hash*(v: T): Hash {.noSideEffect.} =
    for f in fields(v):
      when f is ref:
        when compiles(hash(f[])):
          if not f.isNil:
            mix f[]
          else:
            mix cast[int](cast[pointer](f))
        else:
          mix cast[int](cast[pointer](f))
      else:
        mix f
    result = !$ result

hashObj FullValueObj
hashObj Value
hashObj Type
hashObj InstructionObj

proc `==`*(a, b: FullValueObj): bool {.noSideEffect.}
proc `==`*(a, b: Value): bool {.noSideEffect.}
proc `==`*(a, b: Type): bool {.noSideEffect.}
proc `==`*(a, b: InstructionObj): bool {.noSideEffect.}
proc `==`*(a, b: StatementObj): bool {.noSideEffect.}

equals *(ref Value)
equals *(ref FullValueObj)
equals *(ref Type)
equals *(ref InstructionObj)
equals *(ref StatementObj)

equals *Value
equals *FullValueObj
equals *Type
equals *TypeMatch
equals *InstructionObj
equals *StatementObj

import strutils

proc `$`*(t: TypeParameter): string {.inline.} = t.name

proc `$`*(t: Type): string

proc `$`*(vt: Box[Type]): string =
  if vt.isNil:
    "None"
  else: $vt.unbox

proc `$`*(value: Value): string

proc `$`*(value: FullValue): string =
  result = case value.kind
  of vkNone: "()"
  of vkInt32: $value.int32Value
  of vkUint32: $value.uint32Value
  of vkFloat32: $value.float32Value
  of vkBool: $value.boolValue
  of vkEffect: "Effect(" & $value.effectValue.unref & ")"
  of vkReference: "ref(" & $value.referenceValue.unref & ")"
  of vkBoxed: $value.boxedValue
  of vkInt64: $value.int64Value
  of vkUint64: $value.uint64Value
  of vkFloat64: $value.float64Value
  of vkList: ($value.listValue.unref)[1..^1]
  of vkString: value.stringValue.unref
  of vkArray:
    var s = ($value.tupleValue.unref)[1..^1]
    s[0] = '('
    s[^1] = ')'
    s
  of vkType: $value.typeValue
  of vkFunction: "<function>"
  of vkNativeFunction: "<native function>"
  of vkSet: $value.setValue
  of vkTable: $value.tableValue
  of vkExpression: $value.expressionValue[]
  of vkStatement: $value.statementValue[]
  of vkScope: $value.scopeValue[]

proc `$`*(value: Value): string =
  case value.kind
  of vkNone: "()"
  of vkInt32: $value.int32Value
  of vkUint32: $value.uint32Value
  of vkFloat32: $value.float32Value
  of vkBool: $value.boolValue
  of vkReference: "ref(" & $value.referenceValue.unref & ")"
  of vkEffect: "Effect(" & $value.effectValue.unref & ")"
  of boxedValueKinds: $value.boxedValue

proc `$`*(p: TypeBase): string {.inline.} = p.name

proc `$`*(tb: TypeBound): string

proc `$`*(t: Type): string =
  proc `$`(s: seq[Type]): string =
    for t in s:
      if result.len != 0:
        result.add(", ")
      result.add($t)
  result = case t.kind
  of tyNoType: "NoType"
  of tyCompound: t.base.name & "(" & $t.baseArguments & ")"
  of tyAny: "Any"
  of tyNone: "None"
  of tyTuple: "Tuple(" & $t.elements & (if t.unorderedFields.len == 0: "" else: " " & $t.unorderedFields) & (if t.varargs.isNoType: ")" else: ", " & $t.varargs & "...)")
  of tyUnion: "Union(" & $t.operands & ")"
  of tyIntersection: "Intersection(" & $t.operands & ")"
  of tyNot: "Not " & $t.notType
  of tyWithProperty: "WithProperty(" & $t.typeWithProperty & ", " & $t.withProperty & ")"
  of tyBase: "Base(" & $t.typeBase & ")"
  of tySomeValue: "SomeValue(" & $t.someValueType & ")"
  of tyParameter: "Parameter(" & $t.parameter.name & ")"
  of tyValue: "Value(" & $t.value & ": " & $t.valueType & ")"
  #of tyGeneric:
  #  var s = "Generic["
  #  var i = 0
  #  for p, b in t.parameters:
  #    if i != 0: s.add(", ")
  #    else: inc i
  #    s.add(p.name & ": " & $b)
  #  s & "](" & $t.genericPattern[] & ")"
  if t.properties.len != 0:
    result.add(" {") 
    var afterFirst = false
    for tag, arg in t.properties:
      if afterFirst: result.add(", ")
      else: afterFirst = true
      result.add($arg)
      when false:
        if args.len != 0:
          result.add('(')
          for i, arg in args:
            if i != 0: result.add(", ")
            result.add($arg)
          result.add(')')
    result.add('}')

proc `$`*(tb: TypeBound): string =
  (case tb.variance
  of Covariant: '+'
  of Contravariant: '-'
  of Invariant: '~'
  of Ultravariant: '*') & $tb.boundType

proc `$`*(variable: Variable): string =
  variable.name & ": " & $variable.knownType

proc `$`*(context: Context): string =
  result = "context\n"
  for v in context.allVariables:
    result.add("  " & $v & "\n")
  result.add("imports\n")
  for c in context.imports:
    for line in splitLines($c):
      result.add("  " & line & "\n")

proc `$`*(scope: Scope): string =
  result = "scope\n"
  for v in scope.variables:
    result.add("  " & $v & "\n")
  if scope.parent.isNil:
    result.add("imports\n")
    for c in scope.context.imports:
      for line in splitLines($c):
        result.add("  " & line & "\n")
  else:
    result.add("parent ")
    for line in splitLines($scope.parent):
      result.add("  " & line & "\n")

