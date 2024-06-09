package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.fetch.cache.{InMemoryKVStore, Provider}
import au.id.tmm.metazooa.exploring.game.{Move, State}
import au.id.tmm.metazooa.exploring.strategies.Strategy
import au.id.tmm.metazooa.exploring.tree.Tree
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.functor.*

// TODO allow caller to choose strategy for picking from probability distribution.
//  How does it look if you pick mean() vs minimising the worst-case etc
class SmartMostNarrowing[F[_] : Monad] private (
  sizedTreeCache: Provider[F, Tree, SizedTree],
) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    sizedTreeCache
      .get(state.tree)
      .map { cachedSizedTree =>
        val speciesToExclude = state.guesses

        val sizedTree = cachedSizedTree.excluding(speciesToExclude)

        val (bestGuess, _) = GuessScoring.bestGuess(
          NarrowingApproach.MeanLeastRemainingSpecies,
          state,
          sizedTree,
        )

        Move.Guess(bestGuess)
      }

}

object SmartMostNarrowing {

  def apply[F[_] : Monad : Ref.Make]: F[SmartMostNarrowing[F]] =
    for {
      sizedTreeStore <- InMemoryKVStore[F, Tree, SizedTree, SizedTree](Monad[F].pure)
    } yield new SmartMostNarrowing[F](sizedTreeStore.toProvider(tree => Monad[F].pure(SizedTree(tree))))

}
