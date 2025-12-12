package selection.partitioners

import selection.Partitioner
import Ordering.Implicits.*
import Numeric.Implicits.*

type WOut[A, W] = (List[(A, W)], W, List[(A, W)], W, List[(A, W)], W)

object Weighted:
  given weighted[A, W](using ord: Ordering[A], num: Numeric[W]): Partitioner[(A, W), W] with
    def partition(xs: List[(A, W)], pivot: (A, W)): WOut[A, W] =
      xs.foldRight[WOut[A, W]](Nil, num.zero, Nil, num.zero, Nil, num.zero) {
        (x, out) =>
          val (l, lSize, e, eSize, g, gSize) = out
          if x._1 < pivot._1 then (x :: l, lSize + x._2, e, eSize, g, gSize)
          else if x._1 > pivot._1 then (l, lSize, e, eSize, x :: g, gSize + x._2)
          else (l, lSize, x :: e, eSize + x._2, g, gSize)
      }
