package orderstatistic

import datastructures.OrderStatisticTree
import datastructures.OrderStatisticTree.{Empty, Node}

object Insert:
  import Ordering.Implicits.*
  
  def insert[A, M](
    value: A,
    measureOf: A => M,
    combine: (M, M) => M,
    zero: M,
    tree: OrderStatisticTree[A, M]
  )(using Ordering[A]): OrderStatisticTree[A, M] = tree match
    case Empty(_) => Node(measureOf(value), value, Empty(zero), Empty(zero))
    case Node(_, v, l, r) =>
      if value <= v then
        val newLeft = insert(value, measureOf, combine, zero, l)
        val newMeasure = combine(newLeft.measure, combine(measureOf(v), r.measure))
        Node(newMeasure, v, newLeft, r)
      else
        val newRight = insert(value, measureOf, combine, zero, r)
        val newMeasure = combine(l.measure, combine(measureOf(v), newRight.measure))
        Node(newMeasure, v, l, newRight)