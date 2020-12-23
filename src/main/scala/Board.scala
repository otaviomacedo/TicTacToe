object Symbol extends Enumeration {
  type Symbol = Value
  val X, O = Value
}
import Board.isPerfectSquare
import Symbol.{Symbol, _}

object Board {
  def empty(n: Int) = new Board(Array.fill[Option[Symbol]](n * n)(None))

  private def isPerfectSquare(n: Int): Boolean = {
    val closestRoot = Math.sqrt(n)
    n == closestRoot * closestRoot
  }
}

case class Board(cells: Seq[Option[Symbol]]) {
  type GameOutcome = Option[Symbol]

  require(isPerfectSquare(cells.length), "Invalid board size")

  private val n = Math.sqrt(cells.length).toInt

  private val firstRow: Seq[Int] = 0 until n
  private def rows: Seq[Seq[Int]] = LazyList.iterate(firstRow, n)(_.map(_ + n))

  private val firstColumn: Seq[Int] = 0 until n map (_ * n)
  private def columns: Seq[Seq[Int]] = LazyList.iterate(firstColumn, n)(_.map(_ + 1))

  private val mainDiagonal: Seq[Int] = 0 until n map (_ * (n + 1))
  private def diagonals: Seq[Seq[Int]] = LazyList.iterate(mainDiagonal, 2)(_.map(x => x + (n * n - 1 - 2 * x) / (n + 1)))

  def mark(symbol: Symbol, index: Int): Board = {
    require(cells(index).isEmpty, s"Position $index already marked")
    Board(cells.updated(index, Some(symbol)))
  }

  def possibleMoves(symbol: Symbol): Seq[Board] =
    cells.indices
      .filter(cells(_).isEmpty)
      .map(mark(symbol, _))

  def isFinal: Boolean = outcome.isDefined || cells.forall(_.isDefined)

  lazy val outcome: GameOutcome = {
    def winner(positions: Seq[Int]): GameOutcome = positions map cells reduce {
      (a, b) => if (a == b) a else None
    }

    ((rows ++ columns ++ diagonals) map winner find (_.isDefined)).flatten
  }

  override def toString: String = {
    val horizontalSeparator = Array
      .fill[String](n)("-")
      .mkString("\n", "-|-", "\n")

    rows.map(_                       // For each row:
      .map(cells(_) getOrElse " ")   //   Map the positions to symbols
      .mkString(" | ")               //   Generate the string for the row
    ).mkString(horizontalSeparator)  // Generate the string for the whole board
  }
}
