package orderstatistic.tree

import algebras.Monoid
import OrderStatisticTree.{Empty, Node}

object Insertion:
  import Ordering.Implicits.*
  
  def insert[A, M](
    key: A,
    selfM: M,
    tree: OrderStatisticTree[A, M]
  )(using o: Ordering[A], m: Monoid[M]): OrderStatisticTree[A, M] = tree match
    case Empty(_) => Node(key, selfM, m.empty, Empty(m.empty), Empty(m.empty))
    case Node(k, sM, sbtM, l, r) =>
      if key <= k then
        val left = insert(key, selfM, l)
        val measure = m.combine(left.measure, m.combine(sM, r.measure))
        Node(k, sM, measure, left, r)
      else
        val right = insert(key, selfM, r)
        val measure = m.combine(l.measure, m.combine(sM, right.measure))
        Node(k, sM, measure, l, right)