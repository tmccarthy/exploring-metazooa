package au.id.tmm.metazooa.exploring.game

import au.id.tmm.collections.DupelessSeq
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}

final case class State(
  tree: Tree,
  answer: Species,
  guesses: Set[Species],
  hints: Set[Clade],
) {
  def isComplete: Boolean = guesses.contains(answer)
  def hintsAvailable: Boolean = Game.doMove(this, Move.Hint) match {
    case Left(Move.RejectionReason.GameComplete | Move.RejectionReason.NoHintsAvailable) => false
    case Left(_) | Right(_)                                                              => true
  }

  private[game] def visibleCladesOrderedByProximityToGuess: DupelessSeq[Clade] = {
    val visibleClades =
      Set(tree.root) ++ guesses.map(guess => Tree.unsafeGet(tree.mostRecentSharedClade(guess, answer))) ++ hints

    DupelessSeq.from(visibleClades).sorted(tree.proximityTo(answer))
  }

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

  private def doHint(state: State): Either[Move.RejectionReason.NoHintsAvailable.type, State] = {
    val closestRevealedClade = state.visibleCladesOrderedByProximityToGuess.head

    val potentialHint = closestRevealedClade.children
      .collect { case c: Clade =>
        c
      }
      .minOption(state.tree.proximityTo(state.answer))

    potentialHint match {
      case None => Left(Move.RejectionReason.NoHintsAvailable)
      case Some(hint) =>
        if (state.hints.contains(hint)) {
          Left(Move.RejectionReason.NoHintsAvailable) // not sure this is possible
        } else {
          Right(state.copy(hints = state.hints + hint))
        }
    }
  }

  private def doGuess(state: State, guess: Species): Either[Move.RejectionReason.AlreadyGuessed.type, State] =
    if (!state.tree.contains(guess)) {
      throw Tree.NotInTreeError(guess) // TODO probably could improve the error types to make this cleaner
    } else if (state.guesses.contains(guess)) {
      Left(Move.RejectionReason.AlreadyGuessed)
    } else {
      Right(state.copy(guesses = state.guesses + guess))
    }

}
