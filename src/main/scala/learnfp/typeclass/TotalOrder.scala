package learnfp.typeclass

trait TotalOrder[A] {
  def less(lhs: A, rhs: A): Boolean
}

object TotalOrderInstances {
  implicit val intInstance: TotalOrder[Int] =
    (lhs: Int, rhs: Int) => lhs < rhs

  implicit val stringInstance: TotalOrder[String] =
    (lhs: String, rhs: String) => lhs < rhs

  implicit def listInstance[T](
      implicit suborder: TotalOrder[T]): TotalOrder[List[T]] =
    (lhs: List[T], rhs: List[T]) =>
      lhs.zip(rhs).forall(p => suborder.less(p._1, p._2))

}

object Comparator {
  @annotation.implicitNotFound("No instance of TotalOrder found")
  def less[A](lhs: A, rhs: A)(implicit order: TotalOrder[A]) = {
    order.less(lhs, rhs)
  }
}
