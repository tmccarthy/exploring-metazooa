package au.id.tmm.metazooa.exploring.game

import au.id.tmm.collections.NonEmptyDupelessSeq
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.Ops
import au.id.tmm.metazooa.exploring.tree.{Clade, Species}

object GameUtilities {

  /**
    * All clades revealed to the user, ordered according to their proximity to the correct answer.
    */
  def visibleCladesOrderedByProximityToGuess(state: State): NonEmptyDupelessSeq[Clade] = {
    val visibleClades = NonEmptyDupelessSeq.fromHeadTail(
      state.tree.root,
      state.guesses.map(guess => state.tree.mostRecentSharedClade(guess, state.answer).unsafeGet) ++ state.hints,
    )

    visibleClades.sorted(state.tree.proximityTo(state.answer))
  }

  /**
    * The closest reveled clade to the answer. This will be the clade showing as the parent of the "???" node
    */
  def closestRevealedClade(state: State): Clade = visibleCladesOrderedByProximityToGuess(state).head

  /**
    * All possible species, given what is revealed by the state and assuming that the player knows the full Tree.
    */
  // TODO this isn't incorporating the knowledge you can glean from whether or not a hint is available
  def allPossibleSpecies(state: State.VisibleToPlayer): Set[Species] = {
    import state.tree.syntax.*

    val cladeContainingAnswer = state.closestRevealedClade

    val speciesUnderPossibleClades = cladeContainingAnswer.children
      .flatMap {
        case species: Species => Set(species)
        case clade: Clade =>
          val cladeContainsIncorrectGuess = state.guesses.exists { species =>
            species.lineage.contains(clade)
          }

          if (cladeContainsIncorrectGuess) {
            Set.empty
          } else {
            clade.childSpeciesTransitive
          }
      }

    speciesUnderPossibleClades -- state.guesses
  }

}
