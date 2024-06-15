package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{Move, State}
import au.id.tmm.metazooa.exploring.strategies.Strategy
import cats.Applicative
import cats.syntax.functor.*

class SmartMostNarrowing[F[_] : Applicative] private (
  narrowingApproach: NarrowingApproach[?],
) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] = Applicative[F].pure {
    val (bestGuess, _) = GuessScoring.bestGuess(
      narrowingApproach,
      state,
    )

    Move.Guess(bestGuess)
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Applicative]: F[SmartMostNarrowing[F]] = apply(NarrowingApproach.MeanLeastRemaining)

  def apply[F[_] : Applicative](narrowingApproach: NarrowingApproach[?]): F[SmartMostNarrowing[F]] =
    for {
      _ <- Applicative[F].pure(())
    } yield new SmartMostNarrowing[F](narrowingApproach)

}
