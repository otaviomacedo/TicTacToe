import Symbol.{Symbol, _}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.mockito.MockitoSugar

class GameTest extends AnyFunSuite with MockitoSugar {
  val x: Option[Symbol] = Some(X)
  val o: Option[Symbol] = Some(O)
  val e: Option[Symbol] = None

  val player1: Player = mock[Player]
  val player2: Player = mock[Player]

  test("Game unravelling") {
    when(player1.play(any())).thenReturn(Board(Vector(
      x, e, e,
      e, e, e,
      e, e, e)))

    when(player2.play(any())).thenReturn(Board(Vector(
      x, o, e,
      e, e, e,
      e, e, e)))

    when(player1.play(any())).thenReturn(Board(Vector(
      x, o, e,
      x, e, e,
      e, e, e)))

    when(player2.play(any())).thenReturn(Board(Vector(
      x, o, e,
      x, o, e,
      e, e, e)))

    when(player1.play(any())).thenReturn(Board(Vector(
      x, o, e,
      x, o, e,
      x, e, e)))

    val history = Game(player1, player2).unravel()

    assert(history.last.board == Board(Vector(
      x, o, e,
      x, o, e,
      x, e, e)))

    assert(history.last.board.outcome.contains(Symbol.X))
  }
}
