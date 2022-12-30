template same*(a, b: ref): bool =
  system.`==`(a, b)

template refEquality*(a, b: ref): bool =
  same(a, b) or (not a.isNil and not b.isNil and a[] == b[])

template defineRefEquality*(T: type) {.dirty.} =
  mixin `==`
  proc `==`*(a, b: ref T): bool {.noSideEffect.}  =
    same(a, b) or (not a.isNil and not b.isNil and a[] == b[])

import macros

proc build(val1, val2, body, rl: NimNode, name1, name2: string, forceElse = false): NimNode =
  template useField(f): untyped =
    newBlockStmt(
      newStmtList(
        newLetStmt(ident name1, newDotExpr(copy val1, f)),
        newLetStmt(ident name2, newDotExpr(copy val2, f)),
        copy body))
  result = newStmtList()
  for r in rl:
    case r.kind
    of nnkIdentDefs:
      for f in r[0..^3]:
        result.add(useField(ident repr f))
    of nnkRecCase:
      let kind = ident repr r[0][0]
      result.add(useField(kind))
      result.add quote do:
        if `val1`.`kind` != `val2`.`kind`:
          raise newException(FieldDefect, "case discriminators must be equal for zipFields")
      var cs = newTree(nnkCaseStmt, newDotExpr(copy val1, kind))
      for b in r[1..^1]:
        var nb = copy(b)
        nb[^1] = build(val1, val2, body, nb[^1], name1, name2)
        cs.add(nb)
      if forceElse:
        cs.add(newTree(nnkElse, newTree(nnkDiscardStmt, newEmptyNode())))
      result.add(cs)
    of nnkRecWhen:
      var ws = newTree(nnkWhenStmt)
      for b in r[1..^1]:
        var nb = copy(b)
        nb[^1] = build(val1, val2, body, nb[^1], name1, name2)
        ws.add(nb)
      result.add(ws)
    else: error("unexpected record node " & $r.kind, r)

macro zipFields*(val1, val2: object, name1, name2, body: untyped): untyped =
  var t = val1.getTypeImpl()
  t.expectKind nnkObjectTy
  result = build(val1, val2, body, t[^1], $name1, $name2)

macro zipFields*(forceElse: static bool, val1, val2: object, name1, name2, body: untyped): untyped =
  var t = val1.getTypeImpl()
  t.expectKind nnkObjectTy
  result = build(val1, val2, body, t[^1], $name1, $name2, forceElse)

template defineEquality*[ObjT: object](T: type ObjT) {.dirty.} =
  proc `==`*(a, b: T): bool {.noSideEffect.} =
    zipFields(a, b, aField, bField):
      if aField != bField:
        return false
    return true

template defineEquality*[ObjT: ref object](T: type ObjT) {.dirty.} =
  proc `==`*(a, b: T): bool {.noSideEffect.} =
    if same(a, b): return true
    if a.isNil or b.isNil: return false
    zipFields(a[], b[], aField, bField):
      if aField != bField:
        return false
    return true

when isMainModule:
  type Foo = object
    a: int
    case b: bool
    of false:
      c: string
    else:
      d: float
  
  proc `==`(x, y: Foo): bool =
    if x.b != y.b: return false
    zipFields(x, y, xf, yf):
      if xf != yf:
        return false
    true
  
  doAssert Foo(a: 1, b: false, c: "abc") == Foo(a: 1, b: false, c: "abc")
  doAssert Foo(a: 1, b: false, c: "abc") != Foo(a: 2, b: false, c: "abc")
  doAssert Foo(a: 1, b: false, c: "abc") != Foo(a: 1, b: true, d: 3.14)
  doAssert Foo(a: 1, b: false, c: "abc") != Foo(a: 1, b: false, c: "def")
