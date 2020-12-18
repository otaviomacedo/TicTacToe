import Symbol._
import Util.explicitOrdering

import scala.io.StdIn.readLine

trait Player {
  def play(board: Board): Board
}

class User(symbol: Symbol) extends Player {
  override def play(board: Board): Board = {
    try {
      val move = readLine().toInt
      board.mark(symbol, move)
    } catch {
      case e: Exception =>
        println(e.getMessage)
        play(board)
    }
  }
}

class Computer(symbol: Symbol) extends Player {
  private val myOrdering: Ordering[Option[Symbol.Value]] = explicitOrdering(Some(O), None, Some(X))

  override def play(board: Board): Board = bestMove(symbol, board, myOrdering)._1

  private def bestMove(symbol: Symbol, board: Board, ordering: Ordering[Option[Symbol]]): (Board, Option[Symbol]) = {
    def flip(symbol: Symbol): Symbol = if (symbol == X) O else X

    if (board.isFinal) (board, board.outcome)
    else {
      val moves = board.possibleMoves(symbol).map {
        board => (board, bestMove(flip(symbol), board, ordering.reverse))
      }
      val x = moves.maxBy(_._2._2)(ordering)
      (x._1, x._2._2)
    }
  }
}

class Game {
  def start(): Unit = {
    val user = new User(Symbol.O)
    val computer = new Computer(Symbol.X)
    var board = Board.newInstance(3)

    println(board)
    while (!endOfGame()) {
      playersTurn(user)
      playersTurn(computer)
    }

    println(board.outcome match {
      case Some(X) => "I win!"
      case Some(O) => "You win!"
      case None => "Tie"
    })

    def playersTurn(player: Player): Unit = {
      board = player.play(board)
      println(board)
      println()
    }

    def endOfGame(): Boolean = board.isFinal
  }
}

object Game {
  def main(args: Array[String]): Unit = new Game().start()
}