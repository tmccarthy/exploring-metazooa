package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{Move, State}
import au.id.tmm.metazooa.exploring.strategies.Strategy
import cats.Monad
import cats.syntax.functor.*

class SmartMostNarrowing[F[_] : Monad] private (
  narrowingApproach: NarrowingApproach[?],
) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] = Monad[F].pure {
    val (bestGuess, _) = GuessScoring.bestGuess(
      narrowingApproach,
      state,
    )

    Move.Guess(bestGuess)
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Monad]: F[SmartMostNarrowing[F]] = apply(NarrowingApproach.MeanLeastRemaining)

  def apply[F[_] : Monad](narrowingApproach: NarrowingApproach[?]): F[SmartMostNarrowing[F]] =
    for {
      _ <- Monad[F].pure(())
    } yield new SmartMostNarrowing[F](narrowingApproach)

}
