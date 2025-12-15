package orderstatistic.algebras

trait Monoid[M]:
  def combine(a: M, b:M): M
  def empty: M