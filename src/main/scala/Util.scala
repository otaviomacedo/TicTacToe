import scala.annotation.tailrec

object Util {
  def isPerfectSquare(n: Int): Boolean = {
    val closestRoot = Math.sqrt(n)
    n == closestRoot * closestRoot
  }

  // TODO This is so abstract that I don't even know how to characterize it, except
  //  that B is a semigroup and p is the semigroup operation
  def f[A, B](g: A => A, w: A => B, p: (B, B) => B)(pair: (A, B)): (A, B) = {
    val (a, b) = pair
    (g(a), p(w(g(a)), b))
  }

  def scanF[A](initial:  A, f:  A => A, n:  Int): Seq[A] = {
    @tailrec
    def doit(n: Int, seq: Seq[A], a: A): Seq[A] =
      if (n == 0) seq else doit(n - 1, seq :+ a, f(a))

    doit(n, Vector(), initial)
  }

  @tailrec
  def nTimes[A](n: Int, f: A => A, a: A): A = if (n == 0) a else nTimes(n - 1, f, f(a))

  def explicitOrdering[T](array: Array[T]): Ordering[T] = {
    // TODO Handle the case in which the element is not in the array
    (x: T, y: T) => array.indexOf(x).compareTo(array.indexOf(y))
  }
}
