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
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 2, Rational(1737, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 3, Rational(269*2, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 4, Rational(158, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 5, Rational(31, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 6, Rational(3, 269))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 7, Rational.zero)
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, Int.MaxValue, Rational.zero)

  private val stateAtNeognathae = ActualMetazooaFixtures.stateRevealedToClade(neognathae).visibleToPlayer

  expectedRemainingAfterNGuesses(stateAtNeognathae, 0, Rational(43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 1, Rational(316, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 2, Rational(111, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 3, Rational(38, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 4, Rational(8, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 5, Rational(1, 43))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 6, Rational.zero)
  expectedRemainingAfterNGuesses(stateAtNeognathae, Int.MaxValue, Rational.zero)

}
