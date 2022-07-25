import ".."/[primitives, compilation, values]

import common

module types:
  define "Any", Ty(Any)
  define "None", Ty(None)
  typedTempl "type_of", [Ty(Any)], Type(kind: tyType, typeValue: Ty(Any).box):
    let t = args[0].cachedType
    result = toValue constant(t, Type(kind: tyType, typeValue: t.box))
  {.push hints: off.}
  typedTempl "cast", [Ty(Any), Type(kind: tyType, typeValue: Ty(Any).box)], Ty(Any):
    let newStmt = new(Statement)
    newStmt[] = args[0][]
    newStmt.cachedType = scope.context.evaluateStatic(args[1].toInstruction).typeValue.unbox
    result = toValue newStmt
  {.pop.}
  when false:
    let anyType = Type(kind: tyType, typeValue: Ty(Any))
    fn "functionType", [anyType], anyType:
      result = toValue Type(kind: tyFunction, arguments: seq[Type] @[], returnType: args[0].typeValue)
    fn "functionType", [anyType, anyType], anyType:
      result = toValue Type(kind: tyFunction, arguments: @[args[0].typeValue], returnType: args[1].typeValue)
  # todo: .call / .[] for type arguments
