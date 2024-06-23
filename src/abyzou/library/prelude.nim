import ../vm/[compilation, primitives]

import ./[syntax, numbers, logic, types, collections]

proc prelude*: Scope =
  result = newContext(imports = @[syntax(), numbers(), logic(), types(), collections()]).top
  # todo:
  # modules & module system
  # reflection, metas
  # errors
  # iterators
  # functional stuff
  # comparisons, hashes
  # strings
  # random, json, times, io
