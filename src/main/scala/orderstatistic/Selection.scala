package orderstatistic

import datastructures.OrderStatisticTree
import datastructures.OrderStatisticTree.{Empty, Node}
import orderstatistic.algebras.ResidualMonoid

object Selection:
  @annotation.tailrec
  def select[A, M](
    k: M,
    measureOf: A => M,
    t: OrderStatisticTree[A, M]
  )(using rm: ResidualMonoid[M]): Option[A] = t match
    case Empty(_) => None
    case Node(_, v, l, r) =>
      val less = l.measure
      val lessOrEqual = rm.combine(less, measureOf(v))
      if rm.compare(k, l.measure) < 0 then select(k, measureOf, l)
      else if rm.compare(k, lessOrEqual) <= 0 then Some(v)
      else rm.residual(lessOrEqual, k) match
        case Some(res) => select(res, measureOf, r)
        case None => None
