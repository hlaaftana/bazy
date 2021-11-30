# Package

version       = "0.1.0"
author        = "hlaaftana"
description   = "brother academia"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
skipDirs      = @["src/bazy/disabled"]


# Dependencies

requires "nim >= 1.4.0"

when (compiles do: import nimbleutils):
  import nimbleutils

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/hlaaftana/bazy")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(backends = {c, js, nims}, optionCombos = @[
      "",
      #"--gc:arc -d:danger",
      #"-d:bazyUseUnicode=false",
      #"-d:bazyDoLineColumn=false"
    ])
  else:
    echo "tests task not implemented, need nimbleutils"

task buildall, "builds library and exe":
  echo "building all"
  exec "nim c -d:release --gc:arc --d:useMalloc --outdir:bin src/bazy"
  exec "nim c --app:lib -d:release --gc:arc --d:useMalloc --outdir:bin src/bazy"
  echo "done building"
