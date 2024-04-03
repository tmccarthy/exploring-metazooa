package au.id.tmm.metazooa.exploring.game

import au.id.tmm.collections.DupelessSeq
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}

final case class State(
  tree: Tree,
  answer: Species,
  guesses: Set[Species],
  hints: Set[Clade],
) {
  def isComplete: Boolean     = guesses.contains(answer)
  def hintsAvailable: Boolean = ???

  def cladesCommonWithAnswer: DupelessSeq[Clade] = ???

}

object State {}

sealed trait Move

object Move {
  case object Hint extends Move

  final case class Guess(species: Species) extends Move

  sealed trait RejectionReason

  object RejectionReason {
    case object GameComplete     extends RejectionReason
    case object NoHintsAvailable extends RejectionReason
    case object AlreadyGuessed   extends RejectionReason
  }
}

object Game {

  def doMove(state: State, move: Move): Either[Move.RejectionReason, State] =
    if (state.isComplete) Left(Move.RejectionReason.GameComplete)
    else
      move match {
        case Move.Hint           => doHint(state)
        case Move.Guess(species) => doGuess(state, species)
      }

  private def doHint(state: State): Either[Move.RejectionReason.NoHintsAvailable.type, State] = ???

  private def doGuess(state: State, guess: Species): Either[Move.RejectionReason.AlreadyGuessed.type, State] =
    if (!state.tree.contains(guess)) {
      throw Tree.NotInTreeError(guess) // TODO probably could improve the error types to make this cleaner
    } else if (state.guesses.contains(guess)) {
      Left(Move.RejectionReason.AlreadyGuessed)
    } else {
      Right(state.copy(guesses = state.guesses + guess))
    }

}
