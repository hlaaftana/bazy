import bazy/language/[parser, tokenizer, expressions, tokens]
export parser, tokenizer, expressions, tokens

proc parse*(str: string): Expression =
  var tokenizer = newTokenizer(str)
  result = parser.parse(tokenizer.tokenize())

import bazy/vm/[compilation, primitives, library/prelude]

let Prelude* = prelude()

proc compile*(code: string, libraries = @[Prelude]): Program =
  compile(parse(code), libraries)

proc evaluate*(code: string): Value =
  run(compile(code))

when isMainModule and appType in ["lib", "staticlib"]:
  type Binary* {.exportc, bycopy.} = object
    data*: ptr byte
    length*: cint

  proc toBinary*(str: string): Binary =
    result.length = str.len.cint
    result.data = cast[ptr byte](alloc(str.len))
    for i in 0 ..< str.len:
      cast[ptr byte](cast[int](result.data) + i)[] = str[i].byte

  proc parse*(input: cstring): Binary {.stdcall, exportc, dynlib.} =
    toBinary binary(parse($input))

  proc evaluate*(input: cstring): cstring {.stdcall, exportc, dynlib.} =
    try:
      result = cstring $evaluate($input)
    except Exception as e:
      result = cstring e.msg
elif isMainModule and appType == "console":
  import os

  proc commandLine() =
    let params = commandLineParams()
    if params.len > 0:
      case params[0]
      of "eval", "evaluate":
        var
          input, outputFile: string
        var i = 1
        while i < params.len:
          template nextOrFail(message: string = "expected argument"): string =
            if i + 1 < params.len:
              inc i
              params[i]
            else:
              quit(message)
          case params[i]
          of "--input", "--in", "-i": input = readFile(nextOrFail("expected input file"))
          of "--expression", "-e": input = nextOrFail("expected expression")
          of "--output", "--out", "-o": outputFile = nextOrFail("expected output file")
          inc i
        let res = $evaluate(input)
        if outputFile == "": stdout.write(res)
        else: writeFile(outputFile, res)
      of "parse":
        var
          input, outputFile: string
          binary: bool
        var i = 1
        while i < params.len:
          template nextOrFail(message: string = "expected argument"): string =
            if i + 1 < params.len:
              inc i
              params[i]
            else:
              quit(message)
          case params[i]
          of "--input", "--in", "-i": input = readFile(nextOrFail("expected input file"))
          of "--expression", "-e": input = nextOrFail("expected expression")
          of "--output", "--out", "-o": outputFile = nextOrFail("expected output file")
          of "--binary", "-b": binary = true
          inc i
        let ex = parse(input)
        let res = if binary: $binary(ex) else: $ex
        if outputFile == "": stdout.write(res)
        else: writeFile(outputFile, res)
      else: discard
  
  commandLine()
