import ".."/[primitives, compilation, values]

import common

module logic:
  define "Boolean", Ty(Boolean)
  define "true", true
  define "false", false
  fn "not", [Ty(Boolean)], Ty(Boolean):
    toValue not bool(args[0].integerValue)
