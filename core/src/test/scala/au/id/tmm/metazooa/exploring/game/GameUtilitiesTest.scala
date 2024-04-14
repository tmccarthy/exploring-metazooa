package au.id.tmm.metazooa.exploring.game

import au.id.tmm.utilities.testing.syntax.*
import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures.*
import munit.FunSuite

class GameUtilitiesTest extends FunSuite {

  test("allPossibleGuesses for empty") {
    assertEquals(GameUtilities.allPossibleSpecies(cleanState(answer = human)).size, 269)
  }

  test("allPossibleGuesses for sponge") {
    val state = cleanState(answer = sponge)
      .applyMove(Move.Guess(human))
      .get

    assertEquals(GameUtilities.allPossibleSpecies(state), Set(sponge))
  }

  test("allPossibleGuesses when hint helps") {
    val state = cleanState(answer = human)
      .applyMove(Move.Guess(orangutan))
      .get
      .applyMove(Move.Hint)
      .get

    assertEquals(GameUtilities.allPossibleSpecies(state), Set(human, chimpanzee, bonobo, gorilla))
  }

}
