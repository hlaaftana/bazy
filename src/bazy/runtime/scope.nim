import base

type
  MemorySpace* = ref object
    stackSize*: int
  
  Variable* = ref object
    name*: string
    `type`*: Type
    memoryIndex*: int

  Scope* {.acyclic.} = ref object
    imports*: seq[Scope]
    memorySpace*: MemorySpace
    parent*: Scope
    variables*: seq[Variable]

