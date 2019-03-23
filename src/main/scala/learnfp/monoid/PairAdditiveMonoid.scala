package learnfp.monoid

case class Pair[A, B](a: A, b: B)

object PairAdditiveMonoid {

  implicit def nestedMonoidInstance[A: Monoid, B: Monoid]: Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      override def mzero: Pair[A, B] =
        Pair(implicitly[Monoid[A]].mzero, implicitly[Monoid[B]].mzero)
      override def mappend(lhs: Pair[A, B], rhs: Pair[A, B]): Pair[A, B] =
        Pair(
          implicitly[Monoid[A]].mappend(lhs.a, rhs.a),
          implicitly[Monoid[B]].mappend(lhs.b, rhs.b),
        )
    }
}
