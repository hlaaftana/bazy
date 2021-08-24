from tokenizer import TokenKind

type
  MatchKind* = enum
    mkStartsWith, mkEndsWith, mkEquals
    mkSpecialTokenEquals
    mkCharStartsWith, mkCharEndsWith, mkCharEquals

  Match* = object
    case kind*: MatchKind
    of mkStartsWith, mkEndsWith, mkEquals:
      str*: string
    of mkSpecialTokenEquals:
      tokenKind*: TokenKind
    of mkCharStartsWith, mkCharEndsWith, mkCharEquals:
      ch*: char

  OperatorKind* = enum
    okInfix, okPrefix, okPostfix
    # don't know if the following can work as custom syntax:
    # okInfixTernary ->        _ ? _ : _
    # okPrefixInfix ->         let _ in _
    # okPrefixInfixTernary ->  for _ in _ : _

  OperatorGroupObj* {.acyclic.} = object
    precedence*: int
    matches*: seq[Match]
    kind*: OperatorKind
    next*: ref OperatorGroupObj
  OperatorGroup* = ref OperatorGroupObj

template `<`*(a, b: OperatorGroup | OperatorGroupObj): bool =
  a.precedence < b.precedence

proc newFirstOperatorGroup*(): OperatorGroup =
  new(result)

template sandwich(node: OperatorGroup) =
  new(result)
  result[] = node[]
  node.next = result

proc insertAfterLast*(node: OperatorGroup): OperatorGroup =
  ## simpler proc for when node.next is known to be nil 
  sandwich(node)

proc insertAfter*(node: OperatorGroup): OperatorGroup =
  sandwich(node)
  var n = result
  while not n.isNil:
    inc n.precedence
    n = n.next

proc insertReplace*(node: OperatorGroup) =
  var newNode: OperatorGroup
  new(newNode)
  newNode[] = node[]
  reset(node[])
  node.next = newNode