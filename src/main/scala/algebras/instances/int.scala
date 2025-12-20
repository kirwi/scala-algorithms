package algebras.instances

import algebras.{Monoid, ResidualMonoid}

given Monoid[Int] with
  def combine(a: Int, b: Int): Int = a + b
  def empty: Int = 0

given ResidualMonoid[Int] with
  def combine(a: Int, b: Int): Int = summon[Monoid[Int]].combine(a, b)
  def empty: Int = summon[Monoid[Int]].empty
  def compare(a: Int, b: Int): Int = summon[Ordering[Int]].compare(a, b)
  def residual(p: Int, k: Int): Option[Int] =
    if compare(p, k) <= 0 then Some(k - p) else None