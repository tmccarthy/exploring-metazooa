package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.fetch.cache.{InMemoryKVStore, Provider}
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.functor.*
import spire.math.Rational
import spire.std.int.IntAlgebra

import scala.annotation.tailrec
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
          numRemainingSpeciesAfterGuessing(
            sizedTree.subTreeFrom(state.closestRevealedClade).unsafeGet,
            guess,
          ),
        )
      }

    meanRemainingSpeciesPerGuess.minBy { case (species, averageRemainingSpecies) =>
      (averageRemainingSpecies, species.ncbiId)
    }
  }

  private def numRemainingSpeciesAfterGuessing(
    sizedTree: SizedTree,
    guess: Species,
  ): ProbabilityDistribution[NumSpecies] = {
    val tree = sizedTree.tree
    import tree.syntax.*

    val cladeSize = sizedTree.size.toLong

    val builder = ProbabilityDistribution.builder[NumSpecies]

    // Case where the guess is correct
    builder.addOne(0 -> RationalProbability.makeUnsafe(1L, cladeSize))

    @tailrec
    def go(clade: Clade, countIdentifiedByLessBasalTaxon: NumSpecies): Unit = {
      val identifiedByThisClade: NumSpecies =
        sizedTree.sizeOfClade(clade).unsafeGet - countIdentifiedByLessBasalTaxon

      if (identifiedByThisClade > 0) {
        builder.addOne(identifiedByThisClade -> RationalProbability.makeUnsafe(identifiedByThisClade.toLong, cladeSize))

        clade.parent match {
          case Some(parent) => go(parent, sizedTree.sizeOfClade(clade).unsafeGet)
          case None         => ()
        }
      } else {
        clade.parent match {
          case Some(parent) => go(parent, countIdentifiedByLessBasalTaxon)
          case None         => ()
        }
      }

    }

    guess.parent.foreach(go(_, 1))

    builder.result().getOrThrow
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Monad : Ref.Make]: F[SmartMostNarrowing[F]] =
    for {
      sizedTreeStore <- InMemoryKVStore[F, Tree, SizedTree, SizedTree](Monad[F].pure)
    } yield new SmartMostNarrowing[F](sizedTreeStore.toProvider(tree => Monad[F].pure(SizedTree(tree))))

}
