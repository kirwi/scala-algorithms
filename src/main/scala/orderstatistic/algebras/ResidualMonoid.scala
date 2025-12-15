package orderstatistic.algebras

trait ResidualMonoid[M] extends Monoid[M] with Ordering[M]:
  def residual(p: M, k: M): Option[M]
