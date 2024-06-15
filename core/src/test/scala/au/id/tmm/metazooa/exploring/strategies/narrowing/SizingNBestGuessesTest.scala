package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures.{cleanStateVisibleToPlayer, neognathae}
import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, State}
import au.id.tmm.metazooa.exploring.strategies.MeanNumSpecies
import munit.{FunSuite, Location}
import spire.math.Rational

class SizingNBestGuessesTest extends FunSuite {

  private def expectedRemainingAfterNGuesses(
    initialState: State.VisibleToPlayer,
    nGuesses: Int,
    expectedMeanRemaining: MeanNumSpecies,
  )(implicit
    loc: Location,
  ): Unit =
    test(
      s"Starting at ${initialState.closestRevealedClade.name}, mean number of remaining species after guessing" +
        s" $nGuesses perfect guesses is " + expectedMeanRemaining.toFloat,
    ) {
      val actualNumRemainingSpecies = GuessScoring.expectedRemainingSpeciesAfterNPerfectGuesses(
        NarrowingApproach.MeanLeastRemaining,
        initialState,
        nGuesses,
      )

      assertEquals(actualNumRemainingSpecies, expectedMeanRemaining, actualNumRemainingSpecies.toDouble)
    }

  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 0, Rational(269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 1, Rational(10326, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 2, Rational(2499, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 3, Rational(1085, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 4, Rational(502, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 5, Rational(233, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 6, Rational(150, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 7, Rational(101, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 9, Rational(37, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 10, Rational(17, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 11, Rational(5, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 12, Rational(1, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 13, Rational.zero)
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, Int.MaxValue, Rational.zero)

  private val stateAtNeognathae = ActualMetazooaFixtures.stateRevealedToClade(neognathae).visibleToPlayer

  expectedRemainingAfterNGuesses(stateAtNeognathae, 0, Rational(43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 1, Rational(1078, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 2, Rational(658, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 3, Rational(382, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 4, Rational(210, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 5, Rational(148, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 6, Rational(101, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 7, Rational(65, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 9, Rational(17, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 10, Rational(5, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 11, Rational(1, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 12, Rational.zero)
  expectedRemainingAfterNGuesses(stateAtNeognathae, Int.MaxValue, Rational.zero)

}
