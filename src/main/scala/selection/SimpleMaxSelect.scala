package selection

object SimpleMaxSelect:
  import Ordering.Implicits.*

  @annotation.tailrec
  def select[A](xs: List[A])(using Ordering[A]): A = xs match
    case p :: Nil => p
    case p :: ps =>
      val (l, r) = ps.partition(_ <= p)
      if r.isEmpty then p else select(r)