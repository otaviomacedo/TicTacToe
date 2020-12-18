object Symbol extends Enumeration {
  type Symbol = Value
  val X, O = Value
}
import Symbol.{Symbol, _}
import Util.{isPerfectSquare, scanF}

object Board {
  def empty(n: Int) = new Board(Array.fill[Option[Symbol]](n * n)(None))
}

case class Board(cells: Seq[Option[Symbol]]) {
  type GameOutcome = Option[Symbol]

  if (!isPerfectSquare(cells.length)) {
    throw new Exception("Invalid board size")
  }

  private val n = Math.sqrt(cells.length).toInt
  private val firstRow: Seq[Int] = 0 until n
  private val nextRow = (r: Seq[Int]) => r.map(_ + n)
  private val firstColumn: Seq[Int] = 0 until n map (_ * n)
  private val nextColumn = (r: Seq[Int]) => r.map(_ + 1)
  private val mainDiagonal: Seq[Int] = 0 until n map (_ * (n + 1))
  private val nextDiagonal = (r: Seq[Int]) => r.map(x => x + (n*n - 1 - 2 * x) / (n + 1))

  private def rows: Seq[Seq[Int]] = scanF(firstRow, nextRow, n)
  private def columns: Seq[Seq[Int]] = scanF(firstColumn, nextColumn, n)
  private def diagonals: Seq[Seq[Int]] = scanF(mainDiagonal, nextDiagonal, 2)

  def mark(symbol: Symbol, index: Int): Board = {
    if (cells(index).isDefined)
      throw new Exception(s"Position $index already marked")
    else
      new Board(cells.updated(index, Some(symbol)))
  }

  def possibleMoves(symbol: Symbol): Seq[Board] =
    cells.zipWithIndex.filter(_._1.isEmpty).map(pair => mark(symbol, pair._2))

  def isFinal: Boolean = outcome.isDefined || cells.forall(_.isDefined)

  lazy val outcome: GameOutcome = {
    def product(a: GameOutcome, b: GameOutcome): GameOutcome = if (a == b) a else None
    def winner(array: Seq[Int]): GameOutcome = array map cells reduce product

    Vector(rows, columns, diagonals).flatten map winner find(_.isDefined) getOrElse None
  }

  override def toString: String = {
    val verticalSeparator = Array
      .fill[String](n)("-")
      .mkString("\n", "-|-", "\n")

    rows.map(_                     // For each row:
      .map(cells)                  //   Transform the positions into symbols
      .map(_.getOrElse(" "))       //   Convert the symbols to strings
      .mkString(" | ")             //   Generate the string for the row
    ).mkString(verticalSeparator)  // Generate the string for the whole board
  }
}
