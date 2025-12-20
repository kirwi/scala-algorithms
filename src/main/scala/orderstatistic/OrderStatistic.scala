package orderstatistic

trait OrderStatistic[S]:
  type A
  type M
  def select(s: S, k: M): Option[A]
  def rank(s: S, k: M): M
  def measure(s: S): M

object OrderStatistic:
  extension[S](s: S)(using os: OrderStatistic[S])
    def select(k: os.M): Option[os.A] = os.select(s, k)
    def rank(k: os.M): os.M = os.rank(s, k)
    def measure: os.M = os.measure(s)