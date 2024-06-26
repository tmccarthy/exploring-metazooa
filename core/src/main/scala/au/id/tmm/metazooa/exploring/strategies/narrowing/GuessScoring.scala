package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, NumSpecies}
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*
import spire.math.Rational
import spire.std.int.IntAlgebra

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

// TODO this is not the right name for this object
private[strategies] object GuessScoring {

  // TODO this is a real mess, should be cleaned up
  def expectedRemainingSpeciesAfterNPerfectGuesses(
    approach: NarrowingApproach,
    stateVisibleToPlayer: State.VisibleToPlayer,
    nGuesses: Int,
  ): MeanNumSpecies = {
    val allPossibleSpecies = GameUtilities.allPossibleSpecies(stateVisibleToPlayer)

    val allPossibleAnswers: ProbabilityDistribution[Species] =
      ProbabilityDistribution.allElementsEvenly(allPossibleSpecies) match {
        case Some(distribution) => distribution
        case None               => return Rational.zero
      }

    @tailrec
    def go(state: State, nGuesses: Int): ProbabilityDistribution[MeanNumSpecies] =
      if (state.isComplete) {
        ProbabilityDistribution.always(Rational.zero)
      } else if (nGuesses == 0) {
        // TODO this needs to be done with the sized tree
        ProbabilityDistribution.always(Rational(GameUtilities.allPossibleSpecies(state.visibleToPlayer).size))
      } else {
        val (speciesToGuess, _) = bestGuess(approach, state.visibleToPlayer)

        state.applyMove(Move.Guess(speciesToGuess)) match {
          case Right(newState)                         => go(newState, nGuesses = nGuesses - 1)
          case Left(Move.RejectionReason.GameComplete) => ProbabilityDistribution.always(Rational.zero)
          case Left(otherRejectionReason)              => throw new AssertionError(otherRejectionReason.toString)
        }
      }

    val distributionOfRemaining: ProbabilityDistribution[MeanNumSpecies] = allPossibleAnswers.flatMap { answer =>
      go(
        state = stateVisibleToPlayer.assumingAnswerIs(answer) match {
          case Right(state)                                                          => state
          case Left(e: State.VisibleToPlayer.AssumedAnswerNotInClosestRevealedClade) => throw new AssertionError(e)
        },
        nGuesses,
      )
    }

    approach.map(distributionOfRemaining)

  }

  def bestGuess(
    approach: NarrowingApproach,
    state: State.VisibleToPlayer,
  ): (Species, MeanNumSpecies) = {
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

    approach.reduce(scores)
  }

  def numberOfRemainingSpeciesAfterGuessing(
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
