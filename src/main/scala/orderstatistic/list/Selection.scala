package orderstatistic.list

import algebras.ResidualMonoid
import orderstatistic.list.Partitioner.partition

object Selection:

  @annotation.tailrec
  def select[A, M](
    xs: List[(A, M)],
    k: M
  )(using o: Ordering[A], rm: ResidualMonoid[M]): Option[A] = xs match
    case Nil => None
    case y :: ys =>
      val (left, lMeasure, equal, eMeasure, right, rMeasure) = partition(xs, y)
      val lv = rm.combine(lMeasure, eMeasure)
      if rm.compare(k, lMeasure) < 0 then select(left, k)
      else if rm.compare(k, lv) <= 0 then Some(y._1)
      else rm.residual(lv, k) match
        case None => None
        case Some(res) => select(right, res)
