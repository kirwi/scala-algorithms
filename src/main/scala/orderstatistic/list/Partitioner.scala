package orderstatistic.list

import algebras.Monoid

object Partitioner:
  import Ordering.Implicits.*
  
  private type Part[A, M] = (List[A], M, List[A], M, List[A], M)
  
  def partition[A, M](
    xs: List[(A, M)],
    pivot: (A, M)
  )(using o: Ordering[A], m: Monoid[M]): Part[(A, M), M] =
    xs.foldRight[Part[(A, M), M]](Nil, m.empty, Nil, m.empty, Nil, m.empty) {
      (x, part) =>
        val (l, lMeasure, e, eMeasure, g, gMeasure) = part
        if x._1 < pivot._1 then (x :: l, m.combine(lMeasure, x._2), e, eMeasure, g, gMeasure)
        else if x._1 > pivot._1 then (l, lMeasure, e, eMeasure, x :: g, m.combine(gMeasure, x._2))
        else (l, lMeasure, x :: e, m.combine(eMeasure, x._2), g, gMeasure)
    }
