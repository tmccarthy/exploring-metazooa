package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures
import munit.{FunSuite, Location}
import spire.math.Rational
import spire.std.int.IntAlgebra

class GuessScoringTest extends FunSuite {

  private def makeTest(speciesName: String, expectedMeanRemaining: MeanNumSpecies)(implicit loc: Location): Unit =
    test(s"Mean number of remaining guesses after guessing '$speciesName' is " + expectedMeanRemaining.toFloat) {
      val species = ActualMetazooaFixtures.speciesWithNameUnsafe(speciesName)

      val actualRemainingDistribution =
        GuessScoring.numberOfRemainingSpeciesAfterGuessing(SizedTreeFixtures.actualMetazooaTreeSized, species)

      assertEquals(mean(actualRemainingDistribution), expectedMeanRemaining, expectedMeanRemaining.toDouble)
    }

  makeTest("sea sponge", Rational(71824, 269))
  makeTest("weasel", Rational(10326, 269))

}
