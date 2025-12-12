package selection

trait Partitioner[A, Size]:
  def partition(xs: List[A], pivot: A):
    (
      List[A], Size,
      List[A], Size,
      List[A], Size
    )
