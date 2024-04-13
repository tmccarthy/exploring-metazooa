package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import cats.Applicative

import scala.collection.immutable.ArraySeq

class PickMostNarrowing[F[_] : Applicative] extends Strategy[F] {

  override def proposeMove(state: State): F[Move] = Applicative[F].pure {
    Move.Guess {
      GameUtilities
        .allPossibleGuesses(state)
        .map { guess =>
          val remainingSpeciesDataset: ArraySeq[Int] = GameUtilities
            .allPossibleGuesses(state)
            .to(ArraySeq)
            .flatMap { assumedAnswer =>
              val stateWithAssumedAnswer = state.copy(answer = assumedAnswer)

              stateWithAssumedAnswer.applyMove(Move.Guess(guess)) match {
                case Left(_)         => ArraySeq.empty
                case Right(newState) => ArraySeq(GameUtilities.allPossibleGuesses(newState).size)
              }
            }

          val averageRemainingSpecies =
            remainingSpeciesDataset.sum.doubleValue / remainingSpeciesDataset.size.doubleValue

          guess -> averageRemainingSpecies
        }
        .minBy(_._2)
        ._1
    }
  }

}

object PickMostNarrowing {
  def apply[F[_] : Applicative]: PickMostNarrowing[F] = new PickMostNarrowing()
}
