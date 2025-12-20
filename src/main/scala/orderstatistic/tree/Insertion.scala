package orderstatistic.tree

import algebras.Monoid
import OrderStatisticTree.{Empty, Node}

object Insertion:
  import Ordering.Implicits.*
  
  def insert[A, M](
    value: A,
    measureOf: A => M,
    tree: OrderStatisticTree[A, M]
  )(using o: Ordering[A], m: Monoid[M]): OrderStatisticTree[A, M] = tree match
    case Empty(_) => Node(measureOf(value), value, Empty(m.empty), Empty(m.empty))
    case Node(_, v, l, r) =>
      if value <= v then
        val left = insert(value, measureOf, l)
        val measure = m.combine(left.measure, m.combine(measureOf(v), r.measure))
        Node(measure, v, left, r)
      else
        val right = insert(value, measureOf, r)
        val measure = m.combine(l.measure, m.combine(measureOf(v), right.measure))
        Node(measure, v, l, right)