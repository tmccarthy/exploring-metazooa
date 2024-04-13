package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{Game, Move, State}
import au.id.tmm.utilities.errors.GenericException
import cats.MonadThrow
import cats.syntax.flatMap.*

object Simulator {

  def runOne[F[_]](strategy: Strategy[F], initialState: State)(implicit F: MonadThrow[F]): F[List[Move]] = {
    def go(movesSoFar: List[Move], currentState: State): F[List[Move]] =
      strategy.proposeMove(currentState).flatMap { proposedMove =>
        Game.doMove(currentState, proposedMove) match {
          case Left(rejectionReason) =>
            F.raiseError(GenericException(s"Attempted move $proposedMove, but rejected with $rejectionReason"))
          case Right(newState) => {
            val movesNow = movesSoFar.appended(proposedMove)
            if (newState.isComplete) {
              F.pure(movesNow)
            } else {
              go(movesNow, newState)
            }
          }
        }
      }

    go(movesSoFar = List.empty, initialState)
  }

}
