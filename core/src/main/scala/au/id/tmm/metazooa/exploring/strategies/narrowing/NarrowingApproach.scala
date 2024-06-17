package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, mean}
import au.id.tmm.metazooa.exploring.tree.Species
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.algebra.IsRational

import scala.collection.immutable.ArraySeq

// TODO this is more encapuslating a "cost function" for a particular way of evaluating an approach?
// TODO not convinced this is worth all the overhead
trait NarrowingApproach {

  private[strategies] def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies

  private[strategies] def reduce(scores: ArraySeq[(Species, MeanNumSpecies)]): (Species, MeanNumSpecies)

}

object NarrowingApproach {

  abstract class Simple(rOrdering: Ordering[MeanNumSpecies]) extends NarrowingApproach {
    private val ordering: Ordering[(Species, MeanNumSpecies)] =
      Ordering.by[(Species, MeanNumSpecies), MeanNumSpecies](_._2)(rOrdering).orElseBy(_._1.ncbiId)
    override def reduce(scores: ArraySeq[(Species, MeanNumSpecies)]): (Species, MeanNumSpecies) = scores.min(ordering)
  }

  val MeanLeastRemaining: NarrowingApproach = new Simple(Ordering[MeanNumSpecies]) {
    override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies = mean(distribution)
  }

  val MeanMostRemaining: NarrowingApproach =
    new Simple(Ordering[MeanNumSpecies].reverse) {
      override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies = mean(distribution)
    }

  val BestWorstCase: NarrowingApproach =
    new Simple(Ordering[MeanNumSpecies]) {
      override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies =
        IsRational[A].toRational(distribution.outcomes.max(IsRational[A].toOrdering))
    }

}
