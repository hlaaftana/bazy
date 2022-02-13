import ".."/[primitives, compilation, types, values], ../../language/expressions

import common

module syntax:
  templ "=", 2:
    case args[0].kind
    of Name, Symbol:
      let name = args[0].identifier
      let rhs = compile(scope, args[1], +makeType(Any))
      let v = scope.define(name, rhs.cachedType)
      result = toValue variableSet(v.shallowReference, rhs)
    else: echo "unknown for = ", args[0]
