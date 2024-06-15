package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.strategies.{MeanNumSpecies, mean}
import au.id.tmm.metazooa.exploring.tree.Species
import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.algebra.IsRational

import scala.collection.immutable.ArraySeq

// TODO this is more encapuslating a "cost function" for a particular way of evaluating an approach?
trait NarrowingApproach[R] {

  def map[A : IsRational](distribution: ProbabilityDistribution[A]): R

  def reduce(scores: ArraySeq[(Species, R)]): (Species, R)

}

object NarrowingApproach {

  abstract class Simple[R](rOrdering: Ordering[R]) extends NarrowingApproach[R] {
    private val ordering: Ordering[(Species, R)]                      = Ordering.by[(Species, R), R](_._2)(rOrdering).orElseBy(_._1.ncbiId)
    override def reduce(scores: ArraySeq[(Species, R)]): (Species, R) = scores.min(ordering)
  }

  val MeanLeastRemaining: NarrowingApproach[MeanNumSpecies] = new Simple[MeanNumSpecies](Ordering[MeanNumSpecies]) {
    override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies = mean(distribution)
  }

  val MeanMostRemaining: NarrowingApproach[MeanNumSpecies] =
    new Simple[MeanNumSpecies](Ordering[MeanNumSpecies].reverse) {
      override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies = mean(distribution)
    }

  val BestWorstCase: NarrowingApproach[MeanNumSpecies] = // simple(_.outcomes.max, Ordering[MeanNumSpecies])
    new Simple[MeanNumSpecies](Ordering[MeanNumSpecies]) {
      override def map[A : IsRational](distribution: ProbabilityDistribution[A]): MeanNumSpecies =
        IsRational[A].toRational(distribution.outcomes.max(IsRational[A].toOrdering))
    }

}
