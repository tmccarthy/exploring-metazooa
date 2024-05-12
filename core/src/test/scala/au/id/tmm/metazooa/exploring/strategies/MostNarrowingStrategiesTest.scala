package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, Rules, State}
import cats.Applicative
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF.forAllNoShrinkF
import org.scalacheck.{Gen, Test}

class MostNarrowingStrategiesTest extends CatsEffectSuite with ScalaCheckEffectSuite {

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(3)

  // TODO There is an issue here that I haven't figured out. The SmartMostNarrowing strategy considers Lutrinae (otters)
  //      and Mustela (weasel and mink) as equivalent first moves, with all having x/y or 38.386617100371744 remaining
  //      species. But the brute force strategy gives preference to Lutrinae (x/y or 38.386617100371744), and instead
  //      has Mustela at (x/y or 38.39033457249071).
  test(
    s"${classOf[SmartMostNarrowing[IO]].getSimpleName} is equivalent to ${BruteForceMostNarrowing.getClass.getSimpleName}".fail,
  ) {
    val genAnswers = Gen.oneOf(ActualMetazooaFixtures.human, ActualMetazooaFixtures.bonobo)

    forAllNoShrinkF(genAnswers) { answer =>
      for {
        sut  <- SmartMostNarrowing[IO]
        tree <- ActualMetazooaTree.load
        state = State.initial(Rules.infinite, tree, answer)

        (bruteForceResult, sutResult) <- Applicative[IO].tuple2(
          Simulator.runOne(BruteForceMostNarrowing[IO], state),
          Simulator.runOne(sut, state),
        )
      } yield assertEquals(sutResult.moves, bruteForceResult.moves, s"Answer was $answer")
    }
  }

}
