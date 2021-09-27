# Package

version       = "0.1.0"
author        = "hlaaftana"
description   = "brother academia"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]


# Dependencies

requires "nim >= 1.4.0"

task buildall, "builds library and exe":
  echo "building all"
  exec "nim c -d:release --gc:arc --d:useMalloc --outdir:bin src/brotheracademia"
  exec "nim c --app:lib -d:release --gc:arc --d:useMalloc --outdir:bin src/brotheracademia"
  echo "done building"
