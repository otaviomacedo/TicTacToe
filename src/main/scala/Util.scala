object Util {
  def isPerfectSquare(n: Int): Boolean = {
    val closestRoot = Math.sqrt(n)
    n == closestRoot * closestRoot
  }

  def scanF[A](initial:  A, f:  A => A, n:  Int): Seq[A] = {
    lazy val lines: LazyList[A] = initial #:: (lines map f) take n
    lines
  }

  def explicitOrdering[T](array: T*): Ordering[T] = {
    // TODO Handle the case in which the element is not in the array
    (x: T, y: T) => array.indexOf(x).compareTo(array.indexOf(y))
  }
}
