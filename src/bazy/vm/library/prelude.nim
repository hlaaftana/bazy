import ".."/[compilation, primitives]

import "."/[syntax, numbers]

proc prelude*: Context =
  result = newContext(@[syntax(), numbers()])
