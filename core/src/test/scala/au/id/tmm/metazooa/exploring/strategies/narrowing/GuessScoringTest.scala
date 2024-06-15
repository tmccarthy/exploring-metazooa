package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures
import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, mean}
import munit.{FunSuite, Location}
import spire.math.Rational
import spire.std.int.IntAlgebra

class GuessScoringTest extends FunSuite {

  private def expectedRemainingAfterInitialGuess(
    speciesName: String,
    expectedMeanRemaining: MeanNumSpecies,
  )(implicit
    loc: Location,
  ): Unit =
    test(s"Mean number of remaining guesses after guessing '$speciesName' is " + expectedMeanRemaining.toFloat) {
      val species = ActualMetazooaFixtures.speciesWithNameUnsafe(speciesName)

      val actualRemainingDistribution =
        GuessScoring.numberOfRemainingSpeciesAfterGuessing(ActualMetazooaFixtures.cleanStateVisibleToPlayer, species)

      assertEquals(mean(actualRemainingDistribution), expectedMeanRemaining, expectedMeanRemaining.toDouble)
    }

  expectedRemainingAfterInitialGuess("sea sponge", Rational(71824, 269))
  expectedRemainingAfterInitialGuess("weasel", Rational(10326, 269))

}
