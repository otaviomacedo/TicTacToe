import scala.math.PartialOrdering

trait Semilattice[A] {
  def <>(x: A, y: A): A
  def ordering: PartialOrdering[A]
  def findMaximal(as: A*): A = as.reduceLeft((x: A, y: A) => if (ordering.lteq(x, y)) y else x)
}

object Semilattice {
  def fromPartialOrdering[A](po: PartialOrdering[A]): Semilattice[A] =
    new Semilattice[A] {
      override def <>(x: A, y: A): A = if (po.lteq(x, y)) x else y

      override def ordering: PartialOrdering[A] = po
    }

  def fromOperation[A](p: (A, A) => A): Semilattice[A] =
    new Semilattice[A] {
      override def <>(x: A, y: A): A = p(x, y)

      override def ordering: PartialOrdering[A] = new PartialOrdering[A] {
        override def tryCompare(x: A, y: A): Option[Int] = {
          if (x == y) Some(0)
          else if (x == p(x, y)) Some(-1)
          else if (y == p(x, y)) Some(1)
          else None
        }

        override def lteq(x: A, y: A): Boolean = tryCompare(x, y) exists (_ < 1)
      }
    }
}
