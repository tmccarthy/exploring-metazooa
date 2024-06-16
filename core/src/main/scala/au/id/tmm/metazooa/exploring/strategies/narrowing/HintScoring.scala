package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{GameUtilities, State}
import au.id.tmm.metazooa.exploring.strategies.MeanNumSpecies
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.math.Rational
import spire.std.any.IntAlgebra

object HintScoring {

  def expectedRemainingSpeciesAfterHint(
    approach: NarrowingApproach,
    stateVisibleToPlayer: State.VisibleToPlayer,
  ): MeanNumSpecies = {
    import stateVisibleToPlayer.tree.syntax.*

    val possibleSpeciesDistribution: ProbabilityDistribution[Species] =
      ProbabilityDistribution.allElementsEvenly(GameUtilities.allPossibleSpecies(stateVisibleToPlayer)) match {
        case Some(possibleSpeciesDistribution) => possibleSpeciesDistribution
        case None                              => return Rational.zero
      }

    val potentialCladesForHint = stateVisibleToPlayer.closestRevealedClade.children.collect { case clade: Clade =>
      clade
    }

    val hintCladeDistribution = possibleSpeciesDistribution.map { species =>
      // TODO the AssertionError here can occur
      potentialCladesForHint.find(species.hasAncestor).getOrElse(throw new AssertionError())
    }

    approach.map(hintCladeDistribution.map(_.childSpeciesTransitive.size))
  }

}
