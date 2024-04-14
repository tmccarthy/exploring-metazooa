package au.id.tmm.utilities

import _root_.cats.Foldable
import algebra.CommutativeMonoid
import _root_.cats.Monoid
import _root_.cats.Semigroup
import spire.algebra.Field

final case class PartialMean[A : Semigroup] private (sigma: A, n: Long) {
  def toMean(implicit field: Field[A]): A = Field[A].div(sigma, Field[A].fromBigInt(n))

  def combine(that: PartialMean[A]): PartialMean[A] =
    new PartialMean(Semigroup[A].combine(this.sigma, that.sigma), this.n + that.n)

  def |+|(that: PartialMean[A]): PartialMean[A] = this.combine(that)

  def map[B : Semigroup](f: A => B): PartialMean[B] = new PartialMean[B](f(sigma), n)
}

object PartialMean {

  def empty[A : Monoid]                                = new PartialMean(Monoid[A].empty, n = 0)
  def singleValue[A : Semigroup](a: A): PartialMean[A] = new PartialMean(a, n = 1)
  def fromSeq[A : Monoid](seq: scala.collection.Seq[A]): PartialMean[A] =
    new PartialMean[A](Monoid.combineAll(seq), seq.size.toLong)
  def fold[C[_] : Foldable, A : Monoid](c: C[A]): PartialMean[A] = Foldable[C].foldMap(c)(singleValue[A](_))

  def apply[A : Semigroup](sigma: A, n: Long): Either[NegativeCount, PartialMean[A]] =
    if (n < 0) {
      Left(NegativeCount(n))
    } else {
      Right(new PartialMean(sigma, n))
    }

  final case class NegativeCount(badCount: Long) extends RuntimeException(s"Bad count $badCount")

  implicit def monoid[A : Monoid]: CommutativeMonoid[PartialMean[A]] = new CommutativeMonoid[PartialMean[A]] {
    override def empty: PartialMean[A]                                                = PartialMean.empty[A]
    override def combine(left: PartialMean[A], right: PartialMean[A]): PartialMean[A] = left.combine(right)
  }

}
