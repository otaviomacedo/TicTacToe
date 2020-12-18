import Symbol._
import org.scalatest.funsuite.AnyFunSuite

class BoardTest extends AnyFunSuite {
  test("Initial board should not be final") {
    val board = Board.newInstance(3)
    assert(!board.isFinal)
  }

  test("No winner yet, but lots of symbols already written") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 0).mark(O, 1).mark(X, 2)
      .mark(O, 3).mark(O, 4).mark(X, 5)

    assert(!result.isFinal)
  }

  test("Same elements in a row") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 0)
      .mark(X, 1)
      .mark(X, 2)

    assert(result.isFinal)
    assert(result.outcome.contains(X))
  }

  test("Same elements in a column") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 1).mark(X, 4).mark(X, 7)

    assert(result.isFinal)
    assert(result.outcome.contains(X))
  }

  test("Same elements in a diagonal") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 2)
      .mark(X, 4)
      .mark(X, 6)

    assert(result.isFinal)
    assert(result.outcome.contains(X))
  }

  test("Full board is a final position") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 0).mark(O, 1).mark(X, 2)
      .mark(O, 3).mark(O, 4).mark(X, 5)
      .mark(O, 6).mark(X, 7).mark(O, 8)

    assert(result.isFinal)
    assert(result.outcome.isEmpty)
  }

  test("Possible moves") {
    val board = Board.newInstance(3)

    val result = board
      .mark(X, 1)
      .possibleMoves(O)

    val expected = Vector(
      board.mark(O, 0).mark(X, 1),
      board.mark(X, 1).mark(O, 2),
      board.mark(X, 1).mark(O, 3),
      board.mark(X, 1).mark(O, 4),
      board.mark(X, 1).mark(O, 5),
      board.mark(X, 1).mark(O, 6),
      board.mark(X, 1).mark(O, 7),
      board.mark(X, 1).mark(O, 8))

    assert(result.corresponds(expected) { case (x, y) => x.equals(y) })
  }
}
