package searching

object BinarySearch:
  import Ordering.Implicits.*

  def search[A](xs: List[A], target: A)(using Ordering[A]): Option[Int] =
    @annotation.tailrec
    def go(lo: Int, hi: Int): Option[Int] =
      if lo > hi then None
      else
        val mid = lo + (hi - lo)/2
        val curr = xs(mid)
        if curr == target then Some(mid)
        else if curr > target then go(lo, mid-1)
        else go(mid+1, hi)

    go(0, xs.length-1)

  def searchIter[A](xs: IndexedSeq[A], target: A)(using Ordering[A]): Option[Int] =
    var lo = 0
    var hi = xs.length - 1
    var out: Option[Int] = None

    while lo <= hi do
      val mid = lo + (hi - lo)/2
      val curr = xs(mid)
      if curr == target then {lo = hi + 1; out = Some(mid)}
      else if curr < target then lo = mid + 1
      else hi = mid - 1

    out

  def searchFirst[A](xs: IndexedSeq[A], target: A)(using Ordering[A]): Option[Int] =
    @annotation.tailrec
    def go(lo: Int, hi: Int, found: Option[Int]): Option[Int] =
      if lo > hi then found
      else
        val mid = lo + (hi - lo)/2
        val curr = xs(mid)
        if curr == target then go(lo, mid-1, Some(mid))
        else if curr < target then go(mid+1, hi, found)
        else go(lo, mid-1, found)

    go(0, xs.length - 1, None)

  def searchLast[A](xs: IndexedSeq[A], target: A)(using Ordering[A]): Option[Int] =
    @annotation.tailrec
    def go(lo: Int, hi: Int, found: Option[Int]): Option[Int] =
      if lo > hi then found
      else
        val mid = lo + (hi - lo)/2
        val curr = xs(mid)
        if curr == target then go(mid+1, hi, Some(mid))
        else if curr < target then go(mid+1, hi, found)
        else go(lo, mid-1, found)

    go(0, xs.length - 1, None)

  /** Find the subset of the sorted sequence that is bounded below by target. This subset
   *  is represented by the index of the starting position in the sequence. If target is
   *  larger than every element of the sequence, return the length of the sequence
   * */
  def lowerBound[A](xs: IndexedSeq[A], target: A)(using Ordering[A]): Int =
    @annotation.tailrec
    def go(lo: Int, hi: Int, idx: Int): Int =
      if lo > hi then idx
      else
        val mid = lo + (hi - lo)/2
        val curr = xs(mid)
        if curr < target then go(mid+1, hi, idx)
        else go(lo, mid-1, mid)

    go(0, xs.length - 1, xs.length)

  /** Find the starting index of the subsequence that is strictly bounded below by target. */
  def upperBound[A](xs: IndexedSeq[A], target: A)(using Ordering[A]): Int =
    @annotation.tailrec
    def go(lo: Int, hi: Int, idx: Int): Int =
      if lo > hi then idx
      else
        val mid = lo + (hi - lo)/2
        val curr = xs(mid)
        if curr <= target then go(mid+1, hi, idx)
        else go(lo, mid-1, mid)

    go(0, xs.length - 1, xs.length)