package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.MeanNumSpecies
import au.id.tmm.metazooa.exploring.tree.Clade
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.std.any.IntAlgebra

import scala.collection.immutable.ArraySeq

object HintScoring {

  def expectedRemainingSpeciesAfterHint(
    approach: NarrowingApproach,
    stateVisibleToPlayer: State.VisibleToPlayer,
  ): Either[Move.RejectionReason.NoHintsAvailable.type, MeanNumSpecies] = {
    if (!stateVisibleToPlayer.hintsAvailable) {
      return Left(Move.RejectionReason.NoHintsAvailable)
    }

    val allPossibleSpecies = GameUtilities.allPossibleSpecies(stateVisibleToPlayer)

    val potentialCladesForHint = stateVisibleToPlayer.closestRevealedClade.children.collect { case clade: Clade =>
      clade
    }

    ProbabilityDistribution
      .allElementsEvenly(
        potentialCladesForHint
          .to(ArraySeq)
          .map { clade =>
            clade.childSpeciesTransitive.intersect(allPossibleSpecies).size
          },
      )
      .map(approach.map(_))
      .toRight(Move.RejectionReason.NoHintsAvailable)
  }

}
