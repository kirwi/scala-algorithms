package selection

object QuickSelect:
  import Ordering.Implicits.*
  import Numeric.Implicits.*

  def select[A, W](
    xs: List[A],
    k: W
  )(using
    ord: Ordering[A],
    part: Partitioner[A, W],
    ordW: Ordering[W],
    numW: Numeric[W]
  ): Option[A] = xs match
    case Nil => None
    case y :: ys =>
      val (l, lW, e, eW, g, gW) = part.partition(xs, y)
      if k < gW then select(g, k)
      else if k < gW + eW then Some(y)
      else select(l, k - (eW + gW))