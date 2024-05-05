package au.id.tmm.metazooa.exploring.mucking

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.Rules
import au.id.tmm.metazooa.exploring.strategies.{SmartMostNarrowing, StrategyTester}
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*

object TestStrategy extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      tree     <- ActualMetazooaTree.load
      strategy <- SmartMostNarrowing[IO]
      results  <- StrategyTester.testStrategy(tree, Rules.standard, strategy)
      _ <- results.orderedScores.reverse.traverse { case (species, score) =>
        IO.println(s"${score.asInt} for ${species.name}")
      }
    } yield ()

}
