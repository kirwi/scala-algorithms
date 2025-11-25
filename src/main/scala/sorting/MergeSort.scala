package sorting

object MergeSort:

  /** Pure functional merge sort
   * @param xs List of elements to sort
   * @return a new sorted List
   * */

  def mergeSort[A](xs: List[A])(using Ordering[A]): List[A] =
    if xs.length <= 1 then xs
    else
      val (left, right) = xs.splitAt(xs.length / 2)
      merge(mergeSort(left), mergeSort(right))

  def merge[A](left: List[A], right: List[A])(using Ordering[A]): List[A] =
    import Ordering.Implicits.*
    (left, right) match
      case (Nil, _) => right
      case (_, Nil) => left
      case (l :: ls, r :: rs) => {
        if (l < r) then l :: merge(ls, right)
        else r :: merge(left, rs)
      }



