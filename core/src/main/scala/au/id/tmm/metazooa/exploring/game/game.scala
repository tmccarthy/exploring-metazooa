package au.id.tmm.metazooa.exploring.game

import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}
import au.id.tmm.utilities.errors.ProductException

final case class Rules(
  guessCost: Int,
  hintCost: Int,
  gameOverAt: Option[Int],
)

object Rules {
  val standard: Rules = Rules(guessCost = 1, hintCost = 3, gameOverAt = Some(20))
  val infinite: Rules = standard.copy(gameOverAt = None)
}

final case class State(
  rules: Rules,
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

  def applyMove(move: Move): Either[Move.RejectionReason, State] = Game.doMove(this, move)

  def visibleToPlayer: State.VisibleToPlayer =
    State.VisibleToPlayer(
      rules,
      tree,
      guesses,
      hints,
      closestRevealedClade = GameUtilities.closestRevealedClade(this),
    )
}

object State {

  /**
    * Partial representation of the game state as visible to the player. Does not reveal the answer, but does reveal the
    * least basal clade shared between any hint and the answer.
    */
  final case class VisibleToPlayer(
    rules: Rules,
    tree: Tree,
    guesses: Set[Species],
    hints: Set[Clade],
    closestRevealedClade: Clade,
  ) {
    def assumingAnswerIs(
      assumedAnswer: Species,
    ): Either[VisibleToPlayer.AssumedAnswerNotInClosestRevealedClade, State] =
      if (closestRevealedClade.contains(assumedAnswer)) {
        Right(State(rules, tree, assumedAnswer, guesses, hints))
      } else {
        Left(State.VisibleToPlayer.AssumedAnswerNotInClosestRevealedClade(assumedAnswer, closestRevealedClade))
      }
  }

  object VisibleToPlayer {
    final case class AssumedAnswerNotInClosestRevealedClade(assumedAnswer: Species, closestRevealedClade: Clade)
        extends ProductException
  }
}

sealed trait Move

object Move {
  case object Hint extends Move

  final case class Guess(species: Species) extends Move

  sealed trait RejectionReason

  // TODO needs a "you ran out" reason
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
    val closestRevealedClade = GameUtilities.closestRevealedClade(state)

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
