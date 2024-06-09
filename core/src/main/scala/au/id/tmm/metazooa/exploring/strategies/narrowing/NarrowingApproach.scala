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

  def simple[R](
    mapF: ProbabilityDistribution[NumSpecies] => R,
    rOrdering: Ordering[R],
  ): NarrowingApproach[R] = new NarrowingApproach[R] {
    private val ordering: Ordering[(Species, R)] = Ordering.by[(Species, R), R](_._2)(rOrdering).orElseBy(_._1.ncbiId)

    override def map(distribution: ProbabilityDistribution[NumSpecies]): R = mapF(distribution)
    override def reduce(scores: ArraySeq[(Species, R)]): (Species, R)      = scores.min(ordering)

  }

  val MeanLeastRemaining: NarrowingApproach[MeanNumSpecies] = simple(mean(_), Ordering[MeanNumSpecies])

  val MeanMostRemaining: NarrowingApproach[MeanNumSpecies] = simple(mean(_), Ordering[MeanNumSpecies].reverse)

  val BestWorstCase: NarrowingApproach[MeanNumSpecies] = simple(_.outcomes.max, Ordering[MeanNumSpecies])

}
