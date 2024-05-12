package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.fetch.cache.InMemoryKVStore
import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, Rules, State}
import au.id.tmm.metazooa.exploring.tree.Species
import cats.Applicative
import cats.effect.{IO, Resource}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllNoShrinkF

class MostNarrowingStrategiesTest extends CatsEffectSuite with ScalaCheckEffectSuite {

//  override def scalaCheckTestParameters: Test.Parameters =
//    super.scalaCheckTestParameters.withMinSuccessfulTests(5)

  private val tree = ActualMetazooaFixtures.actualMetazooaTree

  private val genSpecies: Gen[Species] = Gen.oneOf(tree.root.childSpeciesTransitive)

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
    forAllNoShrinkF(genSpecies) { answer =>
      for {
        sut <- SmartMostNarrowing[IO](SmartMostNarrowing.HintRule.NoHints)
        state = State.initial(Rules.infinite, tree, answer)

        (bruteForceResult, sutResult) <- Applicative[IO].tuple2(
          Simulator.runOne(bruteForceMostNarrowingFixture(), state),
          Simulator.runOne(sut, state),
        )
      } yield assertEquals(sutResult.moves, bruteForceResult.moves, s"Answer was $answer")
    }
  }

}
