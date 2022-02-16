## options for tuning

const
  # if using unicode in JS, turn bound checks off
  useUnicode* = block: (const bazyUseUnicode {.booldefine.} = true; bazyUseUnicode)
  doLineColumn* = block: (const bazyDoLineColumn {.booldefine.} = true; bazyDoLineColumn)
  refToken* = block: (const bazyRefToken {.booldefine.} = defined(js); bazyRefToken)
  useArrays* = block: (const bazyUseArrays {.booldefine.} = false; bazyUseArrays)
