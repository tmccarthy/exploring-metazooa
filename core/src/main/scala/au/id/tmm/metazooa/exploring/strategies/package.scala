package au.id.tmm.metazooa.exploring

import au.id.tmm.probability.distribution.exhaustive.ProbabilityDistribution
import spire.algebra.IsRational
import spire.math.Rational

package object strategies {

  type NumSpecies     = Int
  type MeanNumSpecies = Rational

  // TODO put this somewhere shared. Could probably be coupled with PartialMean
  def mean[A : IsRational](distribution: ProbabilityDistribution[A]): Rational =
    distribution.asMap
      .map { case (a, p) =>
        IsRational[A].toRational(a) * p.asRational
      }
      .foldLeft(Rational.zero)(_ + _)

}
