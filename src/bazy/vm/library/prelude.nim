import ".."/[compilation, primitives]

import "."/[syntax, numbers, logic, types]

proc prelude*: Context =
  result = newContext(@[syntax(), numbers(), logic(), types()])
  # todo:
  # reflection, modules, types,
  # functions, logic, errors,
  # comparison, strings, collections/iterators,
  # random, json, times
