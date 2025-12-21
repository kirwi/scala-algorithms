package orderstatistic.tree

import OrderStatisticTree.{Empty, Node}
import algebras.Monoid

object Rank:
  def rank[A, M](tree: OrderStatisticTree[A, M], a: A)(using
    ord: Ordering[A],
    m: Monoid[M]
  ): M = tree match
    case Empty(_) => m.empty
    case Node(k, sM, sbtM, l, r) =>
      if ord.compare(k, a) < 0 then
        m.combine(l.measure, m.combine(sM, rank(r, a)))
      else rank(l, a)
