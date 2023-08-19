import ".."/[compilation, primitives]

import "."/[syntax, numbers, logic, types, collections]

proc prelude*: Context =
  result = newContext(@[syntax(), numbers(), logic(), types(), collections()])
  # todo:
  # reflection, modules, types,
  # functions, errors,
  # comparison, strings, collections/iterators,
  # random, json, times
