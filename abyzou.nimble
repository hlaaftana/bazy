# Package

version       = "0.1.0"
author        = "metagn"
description   = "bagy"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
skipDirs      = @["src/abyzou/disabled"]


# Dependencies

requires "nim >= 1.4.0"
requires "skinsuit >= 0.2.3"

when (compiles do: import nimbleutils):
  import nimbleutils

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/metagn/abyzou")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(optionCombos = @[
      "--gc:refc",
      "--gc:orc",
      #"--gc:orc -d:useMalloc",
      #"--gc:orc -d:danger",
      #"-d:abyzouUseUnicode=false",
      #"-d:abyzouDoLineColumn=false"
    ])
    runTests(@["tests/test_parser.nim"], backends = {js, nims})
  else:
    echo "tests task not implemented, need nimbleutils"

task buildall, "builds library and exe":
  echo "building all"
  exec "nim c -d:release --gc:orc --d:useMalloc --outdir:bin src/abyzou"
  exec "nim c --app:lib -d:release --gc:orc --d:useMalloc --outdir:bin src/abyzou"
  echo "done building"
