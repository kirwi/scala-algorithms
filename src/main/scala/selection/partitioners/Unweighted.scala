package selection.partitioners

import selection.Partitioner
import Ordering.Implicits.*

type UWOut[A] = (List[A], Int, List[A], Int, List[A], Int)

object Unweighted:
  given unweighted[A](using Ordering[A]): Partitioner[A, Int] with
    def partition(xs: List[A], pivot: A): UWOut[A] =
      xs.foldRight[UWOut[A]]((Nil, 0, Nil, 0, Nil, 0)) {
        (x, out) =>
          val (l, lSize, e, eSize, g, gSize) = out
          if x < pivot then (x :: l, lSize + 1, e, eSize, g, gSize)
          else if x > pivot then (l, lSize, e, eSize, x :: g, gSize + 1)
          else (l, lSize, x :: e, eSize, g, gSize)
      }