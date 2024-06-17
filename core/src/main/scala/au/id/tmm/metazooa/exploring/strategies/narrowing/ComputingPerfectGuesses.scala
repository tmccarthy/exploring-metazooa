package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.fetch.cache.{Cache, InMemoryKVStore}
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.narrowing.ComputingPerfectGuesses.SuggestedGuess
import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, NumSpecies}
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.distribution.exhaustive.cats.instances.*
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*
import cats.effect.kernel.Ref
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Eval, Monad, Traverse}
import spire.math.Rational
import spire.std.int.IntAlgebra

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

private[strategies] final class ComputingPerfectGuesses[F[_]] private (
  bestGuessCache: Cache[F, (NarrowingApproach, State.VisibleToPlayer), SuggestedGuess, SuggestedGuess],
)(implicit
  F: Monad[F],
) {

  def expectedRemainingSpeciesAfterNPerfectGuesses(
    approach: NarrowingApproach,
    stateVisibleToPlayer: State.VisibleToPlayer,
    nGuesses: Int,
  ): F[MeanNumSpecies] = {
    def go(state: State, nGuesses: Int): F[ProbabilityDistribution[MeanNumSpecies]] =
      if (state.isComplete) {
        F.pure(ProbabilityDistribution.always(Rational.zero))
      } else if (nGuesses == 0) {
        F.pure(ProbabilityDistribution.always(Rational(GameUtilities.allPossibleSpecies(state.visibleToPlayer).size)))
      } else {
        bestGuess(approach, state.visibleToPlayer).flatMap { suggestedGuess =>
          state.applyMove(suggestedGuess.asMove) match {
            case Right(newState)                         => go(newState, nGuesses = nGuesses - 1)
            case Left(Move.RejectionReason.GameComplete) => F.pure(ProbabilityDistribution.always(Rational.zero))
            case Left(otherRejectionReason)              => throw new AssertionError(otherRejectionReason.toString)
          }
        }
      }

    import ComputingPerfectGuesses.traverseForPD

    (ProbabilityDistribution.allElementsEvenly(GameUtilities.allPossibleSpecies(stateVisibleToPlayer)) match {
      case Some(d) => d
      case None    => return F.pure(Rational.zero)
    })
      .flatTraverse[F, MeanNumSpecies] { species =>
        stateVisibleToPlayer.assumingAnswerIs(species) match {
          case Right(state)                                                          => go(state, nGuesses)
          case Left(e: State.VisibleToPlayer.AssumedAnswerNotInClosestRevealedClade) => throw new AssertionError(e)
        }
      }
      .map(approach.map(_))
  }

  def bestGuess(
    approach: NarrowingApproach,
    state: State.VisibleToPlayer,
  ): F[SuggestedGuess] = bestGuessCache.get((approach, state)) {
    val scores =
      GameUtilities
        .allPossibleSpecies(state)
        .to(ArraySeq)
        .map { guess =>
          val score = approach.map(
            numberOfRemainingSpeciesAfterGuessing(
              state,
              guess,
            ),
          )

          guess -> score
        }

    val (speciesToGuess, expectedNumSpecies) = approach.reduce(scores)

    Monad[F].pure(SuggestedGuess(speciesToGuess, expectedNumSpecies))
  }

  private def numberOfRemainingSpeciesAfterGuessing(
    state: State.VisibleToPlayer,
    guess: Species,
  ): ProbabilityDistribution[NumSpecies] = {
    import state.tree.syntax.*

    val boundingClade     = state.closestRevealedClade
    val boundingCladeSize = sizeOfClade(state, boundingClade).toLong

    val builder = ProbabilityDistribution.builder[NumSpecies]

    // Case where the guess is correct
    builder.addOne(0 -> RationalProbability.makeUnsafe(1L, boundingCladeSize))

    @tailrec
    def go(clade: Clade, countIdentifiedByLessBasalTaxon: NumSpecies): Unit = {
      val identifiedByThisClade: NumSpecies =
        sizeOfClade(state, clade) - countIdentifiedByLessBasalTaxon

      if (identifiedByThisClade > 0) {
        builder.addOne(
          identifiedByThisClade -> RationalProbability.makeUnsafe(identifiedByThisClade.toLong, boundingCladeSize),
        )

        if (clade != boundingClade) {
          clade.parent match {
            case Some(parent) => go(parent, sizeOfClade(state, clade))
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

    builder.result().getOrThrow
  }

  private def sizeOfClade(state: State.VisibleToPlayer, clade: Clade): NumSpecies =
    (clade.childSpeciesTransitive -- state.guesses).size

}

private[strategies] object ComputingPerfectGuesses {

  def apply[F[_] : Monad](
    bestGuessCache: Cache[F, (NarrowingApproach, State.VisibleToPlayer), SuggestedGuess, SuggestedGuess],
  ): ComputingPerfectGuesses[F] =
    new ComputingPerfectGuesses[F](bestGuessCache)

  def backedByInMemoryCache[F[_] : Monad : Ref.Make]: F[ComputingPerfectGuesses[F]] =
    for {
      store <- InMemoryKVStore[F, (NarrowingApproach, State.VisibleToPlayer), SuggestedGuess, SuggestedGuess](
        Monad[F].pure,
      )
      cache = Cache(store)
    } yield apply(cache)

  private[strategies] final case class SuggestedGuess(speciesToGuess: Species, expectedNumSpecies: MeanNumSpecies) {
    def asMove: Move.Guess = Move.Guess(speciesToGuess)
  }

  // TODO move this to the probability library
  // TODO this should be an unordered traverse
  private[ComputingPerfectGuesses] implicit val traverseForPD: Traverse[ProbabilityDistribution] =
    new Traverse[ProbabilityDistribution] {
      override def traverse[G[_] : Applicative, A, B](
        fa: ProbabilityDistribution[A],
      )(
        f: A => G[B],
      ): G[ProbabilityDistribution[B]] =
        fa.asMap
          .to(ArraySeq)
          .traverse { case (a, p) =>
            f(a).map(_ -> p)
          }
          .map(bps => ProbabilityDistribution.apply(bps *).getOrThrow)

      override def foldLeft[A, B](fa: ProbabilityDistribution[A], b: B)(f: (B, A) => B): B =
        fa.outcomes
          .to(Vector)
          .foldLeft(b)(f)

      override def foldRight[A, B](fa: ProbabilityDistribution[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Traverse[Vector].foldRight[A, B](fa.outcomes.to(Vector), lb)(f)
    }

}
