package orderstatistic.tree

enum OrderStatisticTree[+A, M]:
  case Empty(m: M)
  case Node(m: M, value: A, left: OrderStatisticTree[A, M], right: OrderStatisticTree[A, M])

  def measure: M = this match
    case Empty(m) => m
    case Node(m, _, _, _) => m