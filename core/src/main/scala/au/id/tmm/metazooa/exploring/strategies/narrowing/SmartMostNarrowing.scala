package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{Move, State}
import au.id.tmm.metazooa.exploring.strategies.{HintRules, Strategy}
import cats.Monad
import cats.effect.kernel.Ref
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class SmartMostNarrowing[F[_] : Monad] private (
  narrowingApproach: NarrowingApproach,
  hintRules: HintRules,
  perfectGuesses: ComputingPerfectGuesses[F],
) extends Strategy[F] {

  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    hintRules match {
      case HintRules.NoHints => bestMove(state).widen
      case HintRules.HintsAllowed => {
        HintScoring.expectedRemainingSpeciesAfterHint(narrowingApproach, state) match {
          case Right(expectedRemainingAfterHint) =>
            perfectGuesses
              .expectedRemainingSpeciesAfterNPerfectGuesses(narrowingApproach, state, state.rules.hintCost)
              .flatMap[Move] { expectedRemainingAfterEquivalentGuesses =>
                if (expectedRemainingAfterHint <= expectedRemainingAfterEquivalentGuesses) {
                  Monad[F].pure(Move.Hint)
                } else {
                  bestMove(state).widen
                }
              }
          case Left(_) => bestMove(state).widen
        }
      }
    }

  private def bestMove(state: State.VisibleToPlayer): F[Move.Guess] =
    perfectGuesses.bestGuess(narrowingApproach, state).map(_.asMove)

}

object SmartMostNarrowing {

  def apply[F[_] : Monad : Ref.Make](
    hintRules: HintRules = HintRules.NoHints,
    narrowingApproach: NarrowingApproach = NarrowingApproach.MeanLeastRemaining,
  ): F[SmartMostNarrowing[F]] =
    for {
      computingPerfectGuesses <- ComputingPerfectGuesses.backedByInMemoryCache[F]
    } yield new SmartMostNarrowing[F](narrowingApproach, hintRules, computingPerfectGuesses)

}
