import Symbol.{Symbol, _}

import scala.io.StdIn.readLine

trait Player {
  def play(board: Board): Board
}

case class User(symbol: Symbol) extends Player {
  override def play(board: Board): Board = {
    try {
      board.mark(symbol, readLine().toInt)
    } catch {
      case e: Exception =>
        println(e.getMessage)
        play(board)
    }
  }
}

case class Computer(symbol: Symbol) extends Player {
  private val myOrdering: Ordering[Option[Value]] =
    Ordering.by(Array(Some(flip(symbol)), None, Some(symbol)) indexOf _)

  override def play(board: Board): Board = bestMove(symbol, board)(myOrdering)._1

  private def bestMove(symbol: Symbol, board: Board)(implicit ordering: Ordering[Option[Symbol]]): (Board, Option[Symbol]) = {
    if (board.isFinal) (board, board.outcome)
    else {
      val (move, (_, winner)) = (board possibleMoves symbol).map {
        child => (child, bestMove(flip(symbol), child)(ordering.reverse))
      }.maxBy(_._2._2)

      (move, winner)
    }
  }

  private def flip(symbol: Symbol): Symbol = if (symbol == X) O else X
}

case class Game(player1: Player, player2: Player, board: Board) {
  def unravel(): Seq[State] = {
    val initialState = State(board, player1)
    val history: Seq[State] = LazyList.iterate(initialState)(_.next)
    val (intermediateStates, finalStates) = history span (!_.board.isFinal)
    intermediateStates ++ (finalStates take 1)
  }

  case class State(board: Board, player: Player) {
    override def toString: String = {
      lazy val result = "\n" + (board.outcome map (m => s"$m wins!") getOrElse "Tie")
      val footer = if (board.isFinal) result else ""
      board.toString + "\n" + footer
    }

    def next: State = State(player play board, if (player == player1) player2 else player1)
  }
}

object Game extends App {
  val game = Game(Computer(O), Computer(X), Board.empty(3))
  game.unravel() foreach println
}
