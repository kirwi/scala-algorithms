package orderstatistic.tree

enum OrderStatisticTree[+A, M]:
  case Empty(m: M)
  case Node(key: A, selfM: M, subtreeM: M, left: OrderStatisticTree[A, M], right: OrderStatisticTree[A, M])

  def measure: M = this match
    case Empty(m) => m
    case Node(_, _, m, _, _) => m