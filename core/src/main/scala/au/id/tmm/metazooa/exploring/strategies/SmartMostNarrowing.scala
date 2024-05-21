package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.fetch.cache.{Cache, InMemoryKVStore}
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
  sizedTreeCache: Cache[F, Tree, SizedTree, SizedTree],
) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    sizedTreeCache
      .get(state.tree) {
        Monad[F].pure(SizedTree(state.tree))
      }
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
        guess -> mean(numRemainingSpeciesAfterGuessing(sizedTree, state.closestRevealedClade, guess))
      }

    meanRemainingSpeciesPerGuess.minBy { case (species, averageRemainingSpecies) =>
      (averageRemainingSpecies, species.ncbiId)
    }
  }

  private def numRemainingSpeciesAfterGuessing(
    sizedTree: SizedTree,
    boundingClade: Clade,
    guess: Species,
  ): ProbabilityDistribution[NumSpecies] = {
    val tree = sizedTree.tree
    import tree.syntax.*

    val cladeSize = sizedTree.sizeOfClade(boundingClade).unsafeGet.toLong

    val buffer = ArraySeq.newBuilder[(NumSpecies, RationalProbability)] // TODO make the builder public in probability

    // Case where the guess is correct
    buffer.addOne(0 -> RationalProbability.makeUnsafe(1L, cladeSize))

    @tailrec
    def go(clade: Clade, countIdentifiedByLessBasalTaxon: NumSpecies): Unit = {
      val identifiedByThisClade: NumSpecies =
        sizedTree.sizeOfClade(clade).unsafeGet - countIdentifiedByLessBasalTaxon

      if (identifiedByThisClade > 0) {
        buffer.addOne(identifiedByThisClade -> RationalProbability.makeUnsafe(identifiedByThisClade.toLong, cladeSize))

        if (clade != boundingClade) {
          clade.parent match {
            case Some(parent) => go(parent, sizedTree.sizeOfClade(clade).unsafeGet)
            case None         => ()
          }
        }
      } else {
        if (clade != boundingClade) {
          clade.parent match {
            case Some(parent) => go(parent, countIdentifiedByLessBasalTaxon)
            case None         => ()
          }
        }
      }

    }

    guess.parent.foreach(go(_, 1))

    ProbabilityDistribution(buffer.result() *).getOrThrow
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Monad : Ref.Make]: F[SmartMostNarrowing[F]] =
    for {
      sizedTreeCache <- InMemoryKVStore[F, Tree, SizedTree, SizedTree](Monad[F].pure).map(Cache.apply(_))
    } yield new SmartMostNarrowing[F](sizedTreeCache)

}
