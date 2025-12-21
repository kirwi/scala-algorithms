package orderstatistic.tree

import algebras.ResidualMonoid
import orderstatistic.tree.OrderStatisticTree.{Empty, Node}

object Selection:
  @annotation.tailrec
  def select[A, M](
    k: M,
    t: OrderStatisticTree[A, M]
  )(using rm: ResidualMonoid[M]): Option[A] = t match
    case Empty(_) => None
    case Node(key, sM, sbtM, l, r) =>
      val less = l.measure
      val lessOrEqual = rm.combine(less, sM)
      if rm.compare(k, l.measure) < 0 then select(k, l)
      else if rm.compare(k, lessOrEqual) <= 0 then Some(key)
      else rm.residual(lessOrEqual, k) match
        case Some(res) => select(res, r)
        case None => None
