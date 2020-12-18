import Symbol._
import Util.explicitOrdering

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
  private val myOrdering: Ordering[Option[Symbol.Value]] = explicitOrdering(Some(O), None, Some(X))

  override def play(board: Board): Board = bestMove(symbol, board, myOrdering)._1

  private def bestMove(symbol: Symbol, board: Board, ordering: Ordering[Option[Symbol]]): (Board, Option[Symbol]) = {
    def flip(symbol: Symbol): Symbol = if (symbol == X) O else X

    if (board.isFinal) (board, board.outcome)
    else {
      val moves = board.possibleMoves(symbol).map {
        board => (board, bestMove(flip(symbol), board, ordering.reverse))
      }

      val (move, (_, winner)) = moves.maxBy(_._2._2)(ordering)
      (move, winner)
    }
  }
}

case class Game(player1: Player, player2: Player) {

  def unravel(): Seq[State] = {
    val initialState = State(Board.empty(3), player1)
    lazy val states: LazyList[State] = initialState #:: states.map(nextState)
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
      val footer = if (board.isFinal) "\n" + result(board) else ""
      board.toString + "\n" + footer
    }

    private def result(board: Board): String =
      board.outcome map (m => s"$m wins!") getOrElse "Tie"
  }
}

object Game {
  private val user = User(Symbol.O)
  private val computer = Computer(Symbol.X)

  def main(args: Array[String]): Unit =
    Game(user, computer).unravel() foreach println
}
