import Symbol._

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
    Ordering.by(Array(Some(O), None, Some(X)) indexOf _)

  override def play(board: Board): Board = bestMove(symbol, board)(myOrdering)._1

  private def bestMove(symbol: Symbol, board: Board)(implicit ordering: Ordering[Option[Symbol]]): (Board, Option[Symbol]) = {
    def flip(symbol: Symbol): Symbol = if (symbol == X) O else X

    if (board.isFinal) (board, board.outcome)
    else {
      val (move, (_, winner)) = (board possibleMoves symbol).map {
        child => (child, bestMove(flip(symbol), child)(ordering.reverse))
      }.maxBy(_._2._2)

      (move, winner)
    }
  }
}

case class Game(player1: Player, player2: Player, board: Board) {
  def unravel(): Seq[State] = {
    val initialState = State(board, player1)
    val states: Seq[State] = LazyList.iterate(initialState)(nextState)
    val (nonFinalStates, finalStates) = states span (state => !state.board.isFinal)
    nonFinalStates ++ (finalStates take 1)
  }

  private def nextState(current: State): State = {
    def flip(player: Player): Player =
      if (player == player1) player2 else player1

    current match {
      case State(board, player) => State(player play board, flip(player))
    }
  }

  case class State(board: Board, player: Player) {
    override def toString: String = {
      lazy val result = "\n" + (board.outcome map (m => s"$m wins!") getOrElse "Tie")
      val footer = if (board.isFinal) result else ""
      board.toString + "\n" + footer
    }
  }
}

object Game {
  private val user = User(O)
  private val computer = Computer(X)
  private val emptyBoard = Board.empty(3)

  def main(args: Array[String]): Unit = {
    val history = Game(user, computer, emptyBoard).unravel()
    history foreach println
  }
}
