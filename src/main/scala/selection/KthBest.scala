package selection

object KthBest:
  import Ordering.Implicits.*

  private def partitionWithLengths[A](xs: List[A], p: A)(using Ordering[A]):
  ((List[A], Int), (List[A], Int), (List[A], Int)) = xs match
    case Nil => ((Nil, 0), (Nil, 0), (Nil, 0))
    case y :: ys =>
      val ((l, lLen), (e, eLen), (g, gLen)) = partitionWithLengths(ys, p)
      if y < p then ((y :: l, lLen+1), (e, eLen), (g, gLen))
      else if y > p then ((l, lLen), (e, eLen), (y :: g, gLen+1))
      else ((l, lLen), (y :: e, eLen+1), (g, gLen))

  def select[A](xs: List[A], k: Int)(using Ordering[A]): Option[A] =
    if k < 0 || k >= xs.length then None
    else {
      @annotation.tailrec
      def go(xs: List[A], k: Int): A = xs match
        case Nil => throw new IllegalArgumentException("kth best selection of an empty list")
        case p :: ps =>
          val ((l, lLen), (e, eLen), (g, gLen)) = partitionWithLengths(ps, p)
          if k < gLen then go(g, k)
          else if k < eLen + gLen then p
          else go(l, k - (eLen + gLen))
      Some(go(xs, k))
    }
