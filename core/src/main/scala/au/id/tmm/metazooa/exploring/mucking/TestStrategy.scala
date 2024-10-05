package au.id.tmm.metazooa.exploring.mucking

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.Rules
import au.id.tmm.metazooa.exploring.strategies.narrowing.{NarrowingApproach, SmartMostNarrowing}
import au.id.tmm.metazooa.exploring.strategies.{HintRules, Strategy, StrategyTester}
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.traverse.*

object TestStrategy extends IOApp.Simple {

  private def strategyToTest: Resource[IO, Strategy[IO]] =
    Resource.eval {
      SmartMostNarrowing[IO](
        hintRules = HintRules.HintsAllowed,
        narrowingApproach = NarrowingApproach.MeanLeastRemaining,
      )
    }

  override def run: IO[Unit] =
    for {
      tree <- ActualMetazooaTree.load
      results <- strategyToTest.use { strategy =>
        StrategyTester.testStrategy(tree, Rules.standard, strategy)
      }
      _ <- results.orderedScores.reverse.traverse { case (species, score) =>
        IO.println(s"${score.asInt} for ${species.name}")
      }
    } yield ()

}
