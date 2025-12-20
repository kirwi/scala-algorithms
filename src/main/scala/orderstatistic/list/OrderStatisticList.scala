package orderstatistic.list

import orderstatistic.OrderStatistic
import Selection.select
import algebras.ResidualMonoid

final case class OrderStatisticList[A, M](value: List[(A, M)])

object OrderStatisticList:
  given listOrderStatistic[A0, M0](using
    ord: Ordering[A0],
    rm: ResidualMonoid[M0]
  ): OrderStatistic[OrderStatisticList[A0, M0]] with
    type A = A0
    type M = M0
    
    def select(s: OrderStatisticList[A0, M0], k: M0): Option[A0] = Selection.select(s.value, k)
    def rank(s: OrderStatisticList[A0, M0], k: M0): M0 = ???
    def measure(s: OrderStatisticList[A0, M0]): M = s.value.foldLeft(rm.empty) {
      case (acc, (_, m)) => rm.combine(m, acc)
    }