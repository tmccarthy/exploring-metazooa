package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.fetch.cache.{InMemoryKVStore, Provider}
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.{Strategy, mean}
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.metazooa.exploring.tree.{Species, Tree}
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.functor.*
import spire.math.Rational
import spire.std.int.IntAlgebra

import scala.collection.immutable.ArraySeq

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

        val (bestGuess, _) = meanNumRemainingSpeciesAfterBestGuess(state, sizedTree)

        Move.Guess(bestGuess)
      }

  private def meanNumRemainingSpeciesAfterBestGuess(
    state: State.VisibleToPlayer,
    sizedTree: SizedTree,
  ): (Species, Rational) = {
    val meanRemainingSpeciesPerGuess = GameUtilities
      .allPossibleSpecies(state)
      .to(ArraySeq)
      .map { guess =>
        guess -> mean(
          GuessScoring.numberOfRemainingSpeciesAfterGuessing(
            sizedTree.subTreeFrom(state.closestRevealedClade).unsafeGet,
            guess,
          ),
        )
      }

    meanRemainingSpeciesPerGuess.minBy { case (species, averageRemainingSpecies) =>
      (averageRemainingSpecies, species.ncbiId)
    }
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Monad : Ref.Make]: F[SmartMostNarrowing[F]] =
    for {
      sizedTreeStore <- InMemoryKVStore[F, Tree, SizedTree, SizedTree](Monad[F].pure)
    } yield new SmartMostNarrowing[F](sizedTreeStore.toProvider(tree => Monad[F].pure(SizedTree(tree))))

}
