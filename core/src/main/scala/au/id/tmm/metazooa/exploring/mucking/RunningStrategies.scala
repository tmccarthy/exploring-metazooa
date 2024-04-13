package au.id.tmm.metazooa.exploring.mucking

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.game.{Rules, State}
import au.id.tmm.metazooa.exploring.strategies.{PickRandomPossible, Simulator}
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
      moves <- {
        implicit val r: Random[IO] = random
        Simulator.runOne[IO](PickRandomPossible[IO], initialState)
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
