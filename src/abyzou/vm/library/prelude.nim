import ../[compilation, primitives]

import ./[syntax, numbers, logic, types, collections]

proc prelude*: Scope =
  result = newContext(imports = @[syntax(), numbers(), logic(), types(), collections()]).top
  # todo:
  # reflection, modules, types,
  # functions, errors,
  # comparison, strings, collections/iterators,
  # random, json, times
