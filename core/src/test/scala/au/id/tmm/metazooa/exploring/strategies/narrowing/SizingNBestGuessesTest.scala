package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures
import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, SizedTreeFixtures}
import munit.{FunSuite, Location}
import spire.math.Rational

class SizingNBestGuessesTest extends FunSuite {

  private def expectedRemainingAfterNGuesses(
    nGuesses: Int,
    expectedMeanRemaining: MeanNumSpecies,
  )(implicit
    loc: Location,
  ): Unit =
    test(
      s"Mean number of remaining species after guessing $nGuesses perfect guesses is " + expectedMeanRemaining.toFloat,
    ) {
      val actualNumRemainingSpecies = GuessScoring.expectedRemainingSpeciesAfterNPerfectGuesses(
        NarrowingApproach.MeanLeastRemaining,
        ActualMetazooaFixtures.cleanStateVisibleToPlayer,
        SizedTreeFixtures.actualMetazooaTreeSized,
        nGuesses,
      )

      assertEquals(actualNumRemainingSpecies, expectedMeanRemaining, actualNumRemainingSpecies.toDouble)
    }

  expectedRemainingAfterNGuesses(0, Rational(269))
  expectedRemainingAfterNGuesses(1, Rational(10326, 269))
  expectedRemainingAfterNGuesses(2, Rational(2499, 269))
  expectedRemainingAfterNGuesses(3, Rational(1085, 269))
  expectedRemainingAfterNGuesses(4, Rational(502, 269))
  expectedRemainingAfterNGuesses(5, Rational(233, 269))
  expectedRemainingAfterNGuesses(6, Rational(150, 269))
  expectedRemainingAfterNGuesses(7, Rational(101, 269))
  expectedRemainingAfterNGuesses(9, Rational(37, 269))
  expectedRemainingAfterNGuesses(10, Rational(17, 269))
  expectedRemainingAfterNGuesses(11, Rational(5, 269))
  expectedRemainingAfterNGuesses(12, Rational(1, 269))
  expectedRemainingAfterNGuesses(13, Rational.zero)
  expectedRemainingAfterNGuesses(Int.MaxValue, Rational.zero)

}
