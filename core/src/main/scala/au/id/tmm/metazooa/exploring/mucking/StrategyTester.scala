package au.id.tmm.metazooa.exploring.mucking

import au.id.tmm.metazooa.exploring.game.{Rules, Score, State}
import au.id.tmm.metazooa.exploring.strategies.{Simulator, Strategy}
import au.id.tmm.metazooa.exploring.tree.{Species, Tree}
import au.id.tmm.utilities.Fs2Utils
import cats.effect.IO

import scala.collection.immutable.ArraySeq

object StrategyTester {

  final case class Results(
    tree: Tree,
    rules: Rules,
    scorePerSpecies: Species => Score,
  )

  def testStrategy(
    tree: Tree,
    rules: Rules,
    strategy: Strategy[IO],
  ): IO[Results] =
    for {
      allSpecies <- IO.pure(tree.root.childSpeciesTransitive.to(ArraySeq))
      scorePerSpecies <- fs2.Stream
        .emits[IO, Species](allSpecies)
        .map(answer => State.initial(rules, tree, answer))
        .through(Fs2Utils.splitBetweenProcessors(allSpecies.size) { states =>
          states.traverse { state =>
            Simulator.runOne(strategy, state).map(state.answer -> _.score)
          }
        })
        .unchunks
        .compile
        .to(Map)
    } yield Results(tree, rules, scorePerSpecies)

  def initialStates(tree: Tree, rules: Rules): fs2.Stream[IO, State] =
    fs2.Stream
      .emits[IO, Species](tree.root.childSpeciesTransitive.to(ArraySeq))
      .map(answer => State.initial(rules, tree, answer))

}
