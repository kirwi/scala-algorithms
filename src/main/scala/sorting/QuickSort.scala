package sorting

object QuickSort:
  import Ordering.Implicits.*

  private def partitionFold[A](xs: List[A], pivot: A)(using Ordering[A]): (List[A], List[A], List[A]) =
    xs.foldRight[(List[A], List[A], List[A])]((Nil, Nil, Nil))((p, tup) => {
      val (ls, es, rs) = tup
      if p < pivot then (p :: ls, es, rs)
      else if p > pivot then (ls, es, p :: rs)
      else (ls, p :: es, rs)
    })

  def sort[A](xs: List[A])(using Ordering[A]): List[A] =
    xs match
      case Nil | _ :: Nil => xs
      case pivot :: ys =>
        val (lessOrEqual, greaterThan) = ys.partition(_ <= pivot)
        sort(lessOrEqual) ++ (pivot :: sort(greaterThan))

  def sortStable[A](xs: List[A])(using Ordering[A]): List[A] = xs match
    case Nil | _ :: Nil => xs
    case pivot :: tail =>
      @annotation.tailrec
      def partition(left: List[A], equal: List[A], right: List[A], xs: List[A]): List[A] = xs match
        case Nil => sortStable(left.reverse) ++ (pivot :: equal.reverse) ++ sortStable(right.reverse)
        case y :: ys =>
          if y < pivot then partition(y :: left, equal, right, ys)
          else if y > pivot then partition(left, equal, y :: right, ys)
          else partition(left, y :: equal, right, ys)
      partition(Nil, Nil, Nil, tail)

  def foldSort[A](xs: List[A])(using Ordering[A]): List[A] = xs match
    case Nil | _ :: Nil => xs
    case pivot :: tail =>
      val (left, equal, right) = partitionFold(tail, pivot)
      foldSort(left) ++ (pivot :: equal) ++ foldSort(right)

  def sortAcc[A](xs: List[A])(using Ordering[A]): List[A] =
    def go(xs: List[A], acc: List[A]): List[A] = xs match
      case Nil => acc
      case pivot :: tail =>
        val (left, equal, right) = partitionFold(tail, pivot)
        go(left, (pivot :: equal).foldRight(go(right, acc))(_ :: _))

    go(xs, Nil)