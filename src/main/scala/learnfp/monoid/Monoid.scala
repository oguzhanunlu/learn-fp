package learnfp.monoid

trait Monoid[A] {
  def mzero: A
  def mappend(lhs: A, rhs: A): A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  def mzero[A: Monoid]: A = Monoid[A].mzero

  implicit class MonoidOps[A](val lhs: A) extends AnyVal {
    def |+|(rhs: A)(implicit ev: Monoid[A]): A = ev.mappend(lhs, rhs)
  }
}

object MonoidOps {
  import Monoid.MonoidOps
  implicit def toMonoidOps[A: Monoid](x: A): MonoidOps[A] =
    new MonoidOps[A](x)
}
