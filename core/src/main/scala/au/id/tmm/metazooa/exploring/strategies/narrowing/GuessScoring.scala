package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.strategies.NumSpecies
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import au.id.tmm.probability.rational.RationalProbability
import au.id.tmm.utilities.errors.syntax.*

import scala.annotation.tailrec

private[strategies] object GuessScoring {

  def numberOfRemainingSpeciesAfterGuessing(
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
