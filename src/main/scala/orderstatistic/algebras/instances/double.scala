package orderstatistic.algebras.instances

import orderstatistic.algebras.{Monoid, ResidualMonoid}

given Monoid[Double] with 
  def combine(a: Double, b: Double): Double = a + b
  def empty: Double = 0.0
  
given ResidualMonoid[Double] with 
  def combine(a: Double, b: Double): Double = summon[Monoid[Double]].combine(a, b)
  def empty: Double = summon[Monoid[Double]].empty
  def compare(a: Double, b: Double): Int = summon[Ordering[Double]].compare(a, b)
  def residual(p: Double, k: Double): Option[Double] =
    if compare(p, k) <= 0 then Some (k - p) else None