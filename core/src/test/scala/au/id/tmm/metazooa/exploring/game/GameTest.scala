package au.id.tmm.metazooa.exploring.game

import au.id.tmm.utilities.testing.syntax.*
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes.*
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}
import munit.FunSuite

class GameTest extends FunSuite {

  private def makeStateWith(
    answer: Species,
    guesses: Set[Species],
    hints: Set[Clade],
  ): State = State(Rules.infinite, GreatApes.tree, answer, guesses, hints)

  test("wrong guess") {
    val state = makeStateWith(
      answer = human,
      guesses = Set.empty,
      hints = Set.empty,
    )

    val move = Move.Guess(orangutan)

    val expectedState = state.copy(guesses = Set(orangutan))

    assertEquals(Game.doMove(state, move), Right(expectedState))
  }

  test("correct guess") {
    val state = makeStateWith(
      answer = human,
      guesses = Set.empty,
      hints = Set.empty,
    )

    val move = Move.Guess(human)

    val expectedState = state.copy(guesses = Set(human))

    val newState = Game.doMove(state, move)
    assertEquals(newState, Right(expectedState))
    assert(newState.get.isComplete)
  }

  test("re-guess") {
    val state = makeStateWith(
      answer = human,
      guesses = Set(orangutan),
      hints = Set.empty,
    )

    assertEquals(Game.doMove(state, Move.Guess(orangutan)), Left(Move.RejectionReason.AlreadyGuessed))
  }

  test("guess after complete game") {
    val state = makeStateWith(
      answer = human,
      guesses = Set(orangutan, human),
      hints = Set.empty,
    )

    assertEquals(Game.doMove(state, Move.Guess(gorilla)), Left(Move.RejectionReason.GameComplete))
  }

  test("hint") {
    val state = makeStateWith(
      answer = human,
      guesses = Set.empty,
      hints = Set.empty,
    )

    val move = Move.Hint

    val expectedState = state.copy(hints = Set(homininae))

    assertEquals(Game.doMove(state, move), Right(expectedState))
  }

  test("no hints available (hint at bottom level)") {
    val state = makeStateWith(
      answer = bonobo,
      guesses = Set.empty,
      hints = Set(homininae, pan),
    )

    assertEquals(Game.doMove(state, Move.Hint), Left(Move.RejectionReason.NoHintsAvailable))
    assert(!state.hintsAvailable)
  }

  test("no hints available (guess at bottom level)") {
    val state = makeStateWith(
      answer = bonobo,
      guesses = Set(chimpanzee),
      hints = Set.empty,
    )

    assertEquals(Game.doMove(state, Move.Hint), Left(Move.RejectionReason.NoHintsAvailable))
    assert(!state.hintsAvailable)
  }

  test("hint after complete game") {
    val state = makeStateWith(
      answer = human,
      guesses = Set(human),
      hints = Set.empty,
    )

    assertEquals(Game.doMove(state, Move.Hint), Left(Move.RejectionReason.GameComplete))
  }

}
