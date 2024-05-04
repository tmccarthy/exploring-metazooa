package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.fetch.cache.{Cache, InMemoryKVStore}
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.SmartMostNarrowing.{NumSpecies, ProbabilityOps, SizedTree}
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.functor.*
import spire.math.Rational

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

class SmartMostNarrowing[F[_] : Monad] private (getSizedTree: Tree => F[SizedTree]) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    getSizedTree(state.tree).map { cachedSizedTree =>
      val closestRevealedClade = state.closestRevealedClade
      val speciesToExclude     = state.guesses

      val sizedTree = cachedSizedTree.excluding(speciesToExclude)

      val speciesToGuess = GameUtilities
        .allPossibleSpecies(state)
        .to(ArraySeq)
        .map { guess =>
          // TODO fix the syntax here
          guess -> new ProbabilityOps(numRemainingSpeciesAfterGuessing(sizedTree, closestRevealedClade, guess))
            .mean(Rational.apply)
        }
        .minBy(_._2)
        ._1

      Move.Guess(speciesToGuess)
    }

  private def numRemainingSpeciesAfterGuessing(
    sizeTree: SizedTree,
    boundingClade: Clade,
    guess: Species,
  ): ProbabilityDistribution[NumSpecies] = {
    val tree = sizeTree.tree
    import tree.syntax.*

    val cladeSize = sizeTree.sizeOfClade(boundingClade).unsafeGet.toLong

    val buffer = ArraySeq.newBuilder[(NumSpecies, RationalProbability)] // TODO make the builder public in probability

    assert(boundingClade.contains(guess))

    // Case where the guess is correct
    buffer.addOne(0 -> RationalProbability.makeUnsafe(1L, cladeSize))

    @tailrec
    def go(clade: Clade, countIdentifiedByLessBasalTaxon: NumSpecies): Unit = {
      val identifiedByThisClade: NumSpecies =
        sizeTree.sizeOfClade(clade).unsafeGet - countIdentifiedByLessBasalTaxon

      if (identifiedByThisClade > 0) {
        buffer.addOne(identifiedByThisClade -> RationalProbability.makeUnsafe(identifiedByThisClade.toLong, cladeSize))

        if (clade != boundingClade) {
          clade.parent match {
            case Some(parent) => go(parent, sizeTree.sizeOfClade(clade).unsafeGet)
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
      store <- InMemoryKVStore[F, Tree, SizedTree, SizedTree](Monad[F].pure)
      cache = Cache(store)
    } yield new SmartMostNarrowing[F](tree => cache.get(tree)(Monad[F].pure(SizedTree(tree))))

  private type NumSpecies = Int

  private object NumSpecies {
    def count(species: Set[Species]): NumSpecies = species.size
  }

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
      override def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies] = {
        // TODO optimise this line
        val numExcludedSpeciesInClade =
          NumSpecies.count(excludedSpecies) - NumSpecies.count(excludedSpecies -- clade.childSpeciesTransitive)

        underlying.sizeOfClade(clade).map(_ - numExcludedSpeciesInClade)
      }

      override def tree: Tree = underlying.tree
    }
  }

  // TODO put this somewhere shared. Could probably be coupled with PartialMean
  private implicit class ProbabilityOps[A](distribution: ProbabilityDistribution[A]) {
    def mean(toRational: A => Rational): Rational =
      distribution.asMap
        .map { case (a, p) =>
          toRational(a) * p.asRational
        }
        .foldLeft(Rational.zero)(_ + _)
  }

}
