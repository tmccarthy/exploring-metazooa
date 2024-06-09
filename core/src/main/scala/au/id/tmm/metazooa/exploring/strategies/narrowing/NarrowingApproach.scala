package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, NumSpecies, mean}
import au.id.tmm.metazooa.exploring.tree.Species
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.std.int.IntAlgebra

import scala.collection.immutable.ArraySeq

trait NarrowingApproach[R] {

  def map(distribution: ProbabilityDistribution[NumSpecies]): R

  def reduce(scores: ArraySeq[(Species, R)]): (Species, R)

}

object NarrowingApproach {

  val MeanLeastRemainingSpecies: NarrowingApproach[MeanNumSpecies] = new NarrowingApproach[MeanNumSpecies] {
    override def map(distribution: ProbabilityDistribution[NumSpecies]): MeanNumSpecies = mean(distribution)

    override def reduce(scores: ArraySeq[(Species, MeanNumSpecies)]): (Species, MeanNumSpecies) =
      scores.minBy { case (species, averageRemainingSpecies) =>
        (averageRemainingSpecies, species.ncbiId)
      }
  }

}
