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

  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 0, Rational(329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 1, Rational(15276, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 2, Rational(2512, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 3, Rational(732, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 4, Rational(229, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 5, Rational(54, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 6, Rational(6, 329))
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, 7, Rational.zero)
  expectedRemainingAfterNGuesses(cleanStateVisibleToPlayer, Int.MaxValue, Rational.zero)

  private val stateAtNeognathae = ActualMetazooaFixtures.stateRevealedToClade(neognathae).visibleToPlayer

  expectedRemainingAfterNGuesses(stateAtNeognathae, 0, Rational(50))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 1, Rational(449, 50))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 2, Rational(16, 5))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 3, Rational(29, 25))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 4, Rational(17, 50))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 5, Rational(3, 50))
  expectedRemainingAfterNGuesses(stateAtNeognathae, 6, Rational.zero)
  expectedRemainingAfterNGuesses(stateAtNeognathae, Int.MaxValue, Rational.zero)

}
