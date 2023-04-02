import tables, hashes

type
  CachedFile* = distinct uint32
  
  Info* {.byref.} = object
    file*: CachedFile
    line*, column*: uint16

proc `==`*(a, b: CachedFile): bool {.borrow.}
template hash*(a: CachedFile): bool = hash(uint32(a))

var
  fileCacheVar: Table[CachedFile, tuple[filename: string, filenameHash: Hash]] # !global

proc getCachedFile*(filename: string): CachedFile =
  let hash = hash(filename)
  for id, (fn, h) in fileCacheVar:
    if hash == h and filename == fn:
      return id
  result = CachedFile(fileCacheVar.len)
  fileCacheVar[result] = (filename, hash)

proc `$`*(a: CachedFile): string =
  fileCacheVar[a].filename

proc `$`*(info: Info): string =
  result = $info.file & "(" & $info.line & ", " & $info.column
    