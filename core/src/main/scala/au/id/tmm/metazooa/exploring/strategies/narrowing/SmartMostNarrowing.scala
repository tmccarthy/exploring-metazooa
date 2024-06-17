package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{Move, State}
import au.id.tmm.metazooa.exploring.strategies.{HintRules, Strategy}
import cats.Applicative
import cats.syntax.functor.*

class SmartMostNarrowing[F[_] : Applicative] private (
  narrowingApproach: NarrowingApproach,
  hintRules: HintRules,
) extends Strategy[F] {

  override def proposeMove(state: State.VisibleToPlayer): F[Move] = Applicative[F].pure {
    hintRules match {
      case HintRules.NoHints => bestMove(state)
      case HintRules.HintsAllowed => {
        HintScoring.expectedRemainingSpeciesAfterHint(narrowingApproach, state) match {
          case Right(expectedRemainingAfterHint) => {
            val expectedRemainingAfterEquivalentGuesses =
              GuessScoring.expectedRemainingSpeciesAfterNPerfectGuesses(narrowingApproach, state, state.rules.hintCost)

            if (expectedRemainingAfterHint <= expectedRemainingAfterEquivalentGuesses) {
              Move.Hint
            } else {
              bestMove(state)
            }
          }
          case Left(_) => bestMove(state)
        }
      }
    }
  }

  private def bestMove(state: State.VisibleToPlayer): Move.Guess = {
    val (bestGuess, _) = GuessScoring.bestGuess(
      narrowingApproach,
      state,
    )

    Move.Guess(bestGuess)
  }

}

object SmartMostNarrowing {

  def apply[F[_] : Applicative](
    hintRules: HintRules = HintRules.NoHints,
    narrowingApproach: NarrowingApproach = NarrowingApproach.MeanLeastRemaining,
  ): F[SmartMostNarrowing[F]] =
    for {
      _ <- Applicative[F].pure(())
    } yield new SmartMostNarrowing[F](narrowingApproach, hintRules)

}
