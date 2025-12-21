package orderstatistic.list

import algebras.Monoid

object Rank:
  def rank[A, M](xs: List[(A, M)], a: A)(using 
    ord: Ordering[A], 
    m: Monoid[M]
  ): M = xs.foldLeft(m.empty) {
    case (acc, (x, meas)) =>
      if ord.compare(x, a) < 0 then m.combine(acc, meas)
      else acc
  }
