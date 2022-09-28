## options for tuning

const
  # if using unicode in JS, turn bound checks off
  useUnicode* = block: (const bazyUseUnicode {.booldefine.} = true; bazyUseUnicode)
    ## treat unicode alpha and whitespace characters accordingly in tokenizer
  doLineColumn* = block: (const bazyDoLineColumn {.booldefine.} = true; bazyDoLineColumn)
  refToken* = block: (const bazyRefToken {.booldefine.} = defined(js); bazyRefToken)
    ## make token type a reference rather than a value type
  useArrays* = block: (const bazyUseArrays {.booldefine.} = false; bazyUseArrays)
  arraysEmbedLength* = block: (const bazyArraysEmbedLength {.booldefine.} = false; bazyArraysEmbedLength)
  disableUnlikelyCycles* = block: (const bazyDisableUnlikelyCycles {.booldefine.} = false; bazyDisableUnlikelyCycles)
