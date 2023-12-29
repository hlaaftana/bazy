## options for tuning

const
  # if using unicode in JS, turn bound checks off
  useUnicode* = block: (const abyzouUseUnicode {.booldefine.} = true; abyzouUseUnicode)
    ## treat unicode alpha and whitespace characters accordingly in tokenizer
  doLineColumn* = block: (const abyzouDoLineColumn {.booldefine.} = true; abyzouDoLineColumn)
  refToken* = block: (const abyzouRefToken {.booldefine.} = defined(js); abyzouRefToken)
    ## make token type a reference rather than a value type
  arrayImpl* = block: (const abyzouArrayImpl {.strdefine.} = "seq"; abyzouArrayImpl)
