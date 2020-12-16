object Symbol extends Enumeration {
  type Symbol = Value
  val X, O = Value
}
import Symbol.Symbol
import Util.{isPerfectSquare, scanF}

object Board {
  def newInstance(n: Int) = new Board(Array.fill[Option[Symbol]](n * n)(None))
}

class Board(val cells: Seq[Option[Symbol]]) {
  type GameOutcome = Option[Symbol]

  private val semilattice: Semilattice[GameOutcome] =
    Semilattice.fromOperation((x, y) => if (x == y) x else None)

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
    def winner(array: Seq[Int]): GameOutcome = array map cells reduceLeft semilattice.<>

    def checkRows(): GameOutcome = check(firstRow, nextRow, n)

    def checkColumns(): GameOutcome = check(firstColumn, nextColumn, n)

    def checkDiagonals(): GameOutcome = check(mainDiagonal, nextDiagonal, 2)

    def check(initial: Seq[Int], f: Seq[Int] => Seq[Int], n: Int): GameOutcome =
      semilattice.findMaximal(scanF(initial, f, n) map winner:_*)

    semilattice.findMaximal(checkColumns(), checkDiagonals(), checkRows())
  }

  override def toString: String = {
    val verticalSeparator = Array
      .fill[String](n)("-")
      .mkString("\n", "-|-", "\n")

    (scanF(firstRow, nextRow, n) map (_ // Generate all rows
        .map(cells)                     // Transform the positions into symbols
        .map(_.getOrElse(" "))          // Convert the symbols to strings
        .mkString(" | ")                // Generate the string for the row
      )).mkString(verticalSeparator)    // Generate the string for the whole board
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Board]

  override def equals(other: Any): Boolean = other match {
    case that: Board =>
      (that canEqual this) &&
        this.cells.corresponds(that.cells){
          case(x, y) => x.equals(y)
        }
    case _ => false
  }

  override def hashCode(): Int = {
    val state = cells
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
