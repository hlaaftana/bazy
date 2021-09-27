import brotheracademia/[parser, tokenizer, expressions, runtime]
export parser, tokenizer, expressions, runtime

when isMainModule:
  proc binary*(ex: Expression): string =
    result.add(ex.kind.char)
    case ex.kind
    of None: discard
    of Number:
      result.add(ex.number.kind.char)
      result.add(ex.number.bits.char)
      let str = $ex.number
      result.add(str.len.char)
      result.add(str)
    of String:
      let s = ex.str
      result.add(char (s.len shr 24) and 0xFF)
      result.add(char (s.len shr 16) and 0xFF)
      result.add(char (s.len shr 8) and 0xFF)
      result.add(char s.len and 0xFF)
      result.add(s)
    of Name, Symbol:
      let s = ex.identifier
      result.add(char (s.len shr 8) and 0xFF)
      result.add(char s.len and 0xFF)
      result.add(s)
    of Wrapped:
      result.add(binary(ex.wrapped))
    of Dot, Colon:
      result.add(binary(ex.left))
      result.add(binary(ex.right))
    of OpenCall, WrappedCall, Infix, Prefix, Postfix,
       Subscript, CurlySubscript,
       Tuple, Array, Set, Block:
      let exprs =
        case ex.kind
        of OpenCall, WrappedCall, Infix, Prefix, Postfix,
           Subscript, CurlySubscript:
          let args = ex.arguments
          @[ex.address] & args
        of Dot, Colon: @[ex.left, ex.right]
        of Tuple, Array, Set: ex.elements
        of Block: ex.statements
        else: @[]
      result.add(char (exprs.len shr 24) and 0xFF)
      result.add(char (exprs.len shr 16) and 0xFF)
      result.add(char (exprs.len shr 8) and 0xFF)
      result.add(char exprs.len and 0xFF)
      for e in exprs: result.add(binary(e))
  
  when appType in ["lib", "staticlib"]:
    type Binary* {.exportc, bycopy.} = object
      data*: ptr byte
      length*: cint
    proc parse*(input: cstring): Binary {.stdcall, exportc, dynlib.} =
      let output = binary(parse($input))
      result.length = output.len.cint
      result.data = cast[ptr byte](alloc(output.len))
      for i in 0 ..< output.len:
        cast[ptr byte](cast[int](result.data) + i)[] = output[i].byte
  elif appType == "console":
    import os
    let params = commandLineParams()
    if params.len > 0:
      case params[0]
      of "parse":
        var i = 1
        var input, outputFile: string
        template nextOrFail(message: string = "expected argument"): string =
          if i + 1 < params.len:
            inc i
            params[i]
          else:
            quit(message)
        while i < params.len:
          case params[i]
          of "--input", "--in", "-i": input = readFile(nextOrFail("expected input file"))
          of "--expression", "-e": input = nextOrFail("expected expression")
          of "--output", "--out", "-o": outputFile = nextOrFail("expected output file")
          inc i
        let ex = parse(input)
        #echo ex
        let res = $binary(ex)
        #echo (var s = newSeq[byte](res.len); for i in 0 ..< s.len: s[i] = res[i].byte; s)
        if outputFile == "": stdout.write(res)
        else: writeFile(outputFile, res)
      else: discard

when false:
  echo parse("combination(n: Int, r: Int) = \\for result x = 1, each i in 0..<r do while i < r do x = x * (n - i) / (r - i)")
  echo parse("`for` a = b, c = d do e = f")
  echo parse("a := b + c * 4")
