package au.id.tmm.metazooa.exploring.mucking

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.{Rules, State}
import au.id.tmm.metazooa.exploring.strategies.narrowing.{NarrowingApproach, SmartMostNarrowing}
import au.id.tmm.metazooa.exploring.strategies.{Simulator, Strategy}
import au.id.tmm.metazooa.exploring.tree.Tree
import cats.effect.std.Random
import cats.effect.{IO, IOApp, Resource}
import cats.implicits.toTraverseOps

object RunningStrategies extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      random <- Random.scalaUtilRandom[IO]
      tree   <- ActualMetazooaTree.load
      numRuns = 2000
      _ <- makeStrategy.use { strategy =>
        runForRandomInitialState(strategy, tree)(random).replicateA_(numRuns)
      }
    } yield ()

  private val makeStrategy: Resource[IO, Strategy[IO]] =
    for {
      underlyingStrategy <- Resource.eval(SmartMostNarrowing[IO](NarrowingApproach.MeanLeastRemaining))
//      strategyCachePath  <- Resource.eval(IO(Paths.get("cache", "strategy_moves.sql").toAbsolutePath))
//      cachedStrategy     <- CachedPerfectStrategy.cachingAt(strategyCachePath)(underlyingStrategy)
    } yield underlyingStrategy

  private def runForRandomInitialState(strategy: Strategy[IO], tree: Tree)(implicit r: Random[IO]): IO[Unit] =
    for {
      initialState <- generateRandomInitialState(tree)
      _            <- IO.println(s"Answer is ${initialState.answer}")
      moves        <- Simulator.runOne(strategy, initialState)
      _            <- moves.moves.traverse(IO.println)
    } yield ()

  private def generateRandomInitialState(tree: Tree)(implicit r: Random[IO]): IO[State] =
    for {
      answer <- Random[IO].elementOf(tree.root.childSpeciesTransitive)
    } yield State.initial(
      rules = Rules.standard,
      tree,
      answer,
    )

}
