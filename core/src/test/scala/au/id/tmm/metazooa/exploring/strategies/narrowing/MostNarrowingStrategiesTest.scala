package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.fetch.cache.InMemoryKVStore
import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, Rules, State}
import au.id.tmm.metazooa.exploring.strategies.{CachedPerfectStrategy, Simulator}
import au.id.tmm.metazooa.exploring.tree.Species
import cats.Applicative
import cats.effect.{IO, Resource}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen.oneOf
import org.scalacheck.effect.PropF.forAllF

class MostNarrowingStrategiesTest extends CatsEffectSuite with ScalaCheckEffectSuite {

  private val tree = ActualMetazooaFixtures.actualMetazooaTree

  private val speciesPendingIssue1 = ActualMetazooaFixtures.hystricomorpha.childSpeciesTransitive ++
    ActualMetazooaFixtures.muroidea.childSpeciesTransitive
  private val speciesWhereTestIsWorking = tree.root.childSpeciesTransitive -- speciesPendingIssue1

  private val bruteForceMostNarrowingFixture: Fixture[CachedPerfectStrategy[IO, BruteForceMostNarrowing[IO]]] =
    ResourceSuiteLocalFixture(
      classOf[BruteForceMostNarrowing[IO]].getSimpleName,
      for {
        kvStore <- Resource.eval(InMemoryKVStore.SimpleIO[String, String])
        underlying = BruteForceMostNarrowing[IO]
      } yield CachedPerfectStrategy(kvStore, underlying),
    )

  override def munitFixtures: Seq[Fixture[_]] = super.munitFixtures :+ bruteForceMostNarrowingFixture

  test(
    s"${classOf[SmartMostNarrowing[IO]].getSimpleName} is equivalent to ${BruteForceMostNarrowing.getClass.getSimpleName}",
  ) {
    forAllF(oneOf(speciesWhereTestIsWorking)) { answer =>
      assertSmartAndBruteAreEquivalentFor(answer)
    }
  }

  // https://github.com/tmccarthy/exploring-metazooa/issues/1
  test(
    s"${classOf[SmartMostNarrowing[IO]].getSimpleName} is equivalent to ${BruteForceMostNarrowing.getClass.getSimpleName} for rodents".fail,
  ) {
    forAllF(oneOf(speciesPendingIssue1)) { answer =>
      assertSmartAndBruteAreEquivalentFor(answer)
    }
  }

  private def assertSmartAndBruteAreEquivalentFor(answer: Species): IO[Unit] =
    for {
      sut <- SmartMostNarrowing[IO]()
      state = State.initial(Rules.infinite, tree, answer)

      (bruteForceResult, sutResult) <- Applicative[IO].tuple2(
        Simulator.runOne(bruteForceMostNarrowingFixture(), state),
        Simulator.runOne(sut, state),
      )
    } yield assertEquals(sutResult.moves, bruteForceResult.moves, s"Answer was $answer")

}
