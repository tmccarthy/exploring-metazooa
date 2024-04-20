package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import cats.MonadThrow
import cats.effect.std.Random
import cats.syntax.functor.*

class PickRandomPossible[F[_]](implicit r: Random[F], m: MonadThrow[F]) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    for {
      guess <- Random[F].elementOf(GameUtilities.allPossibleSpecies(state))
    } yield Move.Guess(guess)
}

object PickRandomPossible {
  def apply[F[_] : Random : MonadThrow]: PickRandomPossible[F] = new PickRandomPossible()
}
