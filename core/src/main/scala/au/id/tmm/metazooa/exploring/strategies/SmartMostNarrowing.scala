package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.fetch.cache.{Cache, InMemoryKVStore}
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.SmartMostNarrowing.{NumSpecies, SizedTree, mean}
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.functor.*
import spire.algebra.IsRational
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

  private type NumSpecies = Int

  private object NumSpecies {
    def count(species: Set[Species]): NumSpecies = species.size
  }

  // TODO this may be useful in the package
  private sealed trait SizedTree {
    def tree: Tree

    def size: NumSpecies = sizeOfClade(tree.root).unsafeGet

    def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies]

    def excluding(species: Species): SizedTree = this.excluding(Set(species))

    def excluding(species: Set[Species]): SizedTree = if (species.isEmpty) {
      this
    } else {
      this match {
        case pureTree: SizedTree.Pure =>
          SizedTree.WithExclusion(pureTree, species)
        case SizedTree.WithExclusion(underlying, excludedSpecies) =>
          SizedTree.WithExclusion(underlying, excludedSpecies ++ species)
      }
    }
  }

  private object SizedTree {
    def apply(tree: Tree): SizedTree = Pure(tree)

    private final case class Pure(tree: Tree) extends SizedTree {

      private val sizePerClade: Map[Clade, NumSpecies] = {
        val builder = Map.newBuilder[Clade, NumSpecies]

        def go(clade: Clade): Unit = {
          val sizeOfThisClade = NumSpecies.count(clade.childSpeciesTransitive)
          builder.addOne(clade -> sizeOfThisClade)

          clade.children.foreach {
            case clade: Clade => go(clade)
            case _: Species   => ()
          }
        }

        go(tree.root)

        builder.result()
      }

      def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies] =
        sizePerClade.get(clade).toRight(Tree.NotInTreeError(clade))

    }

    private final case class WithExclusion(underlying: SizedTree.Pure, excludedSpecies: Set[Species])
        extends SizedTree {
      private val cladesImpactedByExclusions: Set[Clade] = {
        excludedSpecies.flatMap { species =>
          underlying.tree.lineageOf(species).unsafeGet.cladesRootFirst.toSet
        }
      }

      override def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies] =
        if (cladesImpactedByExclusions.contains(clade)) {
          // TODO optimise this line
          val numExcludedSpeciesInClade =
            NumSpecies.count(excludedSpecies) - NumSpecies.count(excludedSpecies -- clade.childSpeciesTransitive)

          underlying.sizeOfClade(clade).map(_ - numExcludedSpeciesInClade)
        } else {
          underlying.sizeOfClade(clade)
        }

      override def tree: Tree = underlying.tree
    }
  }

  // TODO put this somewhere shared. Could probably be coupled with PartialMean
  def mean[A : IsRational](distribution: ProbabilityDistribution[A]): Rational =
    distribution.asMap
      .map { case (a, p) =>
        IsRational[A].toRational(a) * p.asRational
      }
      .foldLeft(Rational.zero)(_ + _)

}
