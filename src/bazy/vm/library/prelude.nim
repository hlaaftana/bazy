import ".."/[compilation, primitives]

import "."/[syntax, numbers]

proc prelude*: Context =
  result = newContext(@[syntax(), numbers()])
  # todo:
  # reflection, modules, types,
  # functions, logic, errors,
  # comparison, strings, collections/iterators,
  # random, json, times
