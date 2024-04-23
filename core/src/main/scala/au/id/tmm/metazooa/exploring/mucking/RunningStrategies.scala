package au.id.tmm.metazooa.exploring.mucking

import java.nio.file.Paths

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.{Rules, State}
import au.id.tmm.metazooa.exploring.strategies.{CachedPerfectStrategy, PickMostNarrowing, Simulator}
import au.id.tmm.metazooa.exploring.tree.Tree
import cats.effect.std.Random
import cats.effect.{IO, IOApp}
import cats.implicits.toTraverseOps

object RunningStrategies extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      random       <- Random.scalaUtilRandom[IO]
      tree         <- ActualMetazooaTree.load
      initialState <- generateRandomInitialState(tree)(random)
      _            <- IO.println(s"Answer is ${initialState.answer}")
      strategy = PickMostNarrowing[IO]
      strategyCachePath <- IO(
        Paths.get("cache", "strategy_moves.sql").toAbsolutePath,
      ) // TODO absolute path should be in fetch
      moves <- CachedPerfectStrategy
        .cachingAt(strategyCachePath)(strategy)
        .use { strategy =>
          Simulator.runOne[IO](strategy, initialState)
        }
      _ <- moves.traverse(IO.println)
    } yield ()

  private def generateRandomInitialState(tree: Tree)(implicit r: Random[IO]): IO[State] =
    for {
      answer <- Random[IO].elementOf(tree.root.childSpeciesTransative)
    } yield State(
      rules = Rules.standard,
      tree,
      answer,
      guesses = Set.empty,
      hints = Set.empty,
    )

}
