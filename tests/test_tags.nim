import bazy/vm/tags

block:
  let a = tag("a")
  let b = tag("b")
  let secondA = tag("a")
  doAssert a == secondA
  doAssert a != b
  let fakeA = uniqueTag("a")
  let fakeA2 = uniqueTag("a")
  doAssert a != fakeA
  doAssert a != fakeA2
  doAssert fakeA != fakeA2
  let a3 = tag("a")
  doAssert a == a3
  let b2 = tag("b")
  doAssert b == b2
