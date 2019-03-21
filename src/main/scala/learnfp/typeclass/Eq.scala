package learnfp.typeclass

trait Eq[A] {
  def eq(lhs: A, rhs: A): Boolean
}

object Eq {
  def eq[A](lhs: A, rhs: A)(implicit eqt: Eq[A]) = eqt.eq(lhs, rhs)
}

class EqOps[A](lhs: A)(implicit eqt: Eq[A]) {
  def ====(rhs: A): Boolean = eqt.eq(lhs, rhs)
}

object EqOps {
  implicit def toEqOps[A](lhs: A)(implicit eqt: Eq[A]) = new EqOps(lhs)
}

object EqInstances {
  implicit val intEqInstance: Eq[Int] =
    (lhs: Int, rhs: Int) => lhs.equals(rhs)

  implicit val stringEqInstance: Eq[String] =
    (lhs: String, rhs: String) => lhs.equals(rhs)

  implicit def listEqInstance[A](implicit eqt: Eq[A]): Eq[List[A]] =
    (lhs: List[A], rhs: List[A]) => {
      if (lhs.size != rhs.size) false
      else lhs.zip(rhs).forall(p => p._1.equals(p._2))
    }
}
