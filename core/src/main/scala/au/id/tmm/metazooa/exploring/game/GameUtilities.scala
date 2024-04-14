package au.id.tmm.metazooa.exploring.game

import au.id.tmm.collections.NonEmptyDupelessSeq
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}

object GameUtilities {

  /**
    * All clades revealed to the user, ordered according to their proximity to the correct answer.
    */
  def visibleCladesOrderedByProximityToGuess(state: State): NonEmptyDupelessSeq[Clade] = {
    val visibleClades = NonEmptyDupelessSeq.fromHeadTail(
      state.tree.root,
      state.guesses.map(guess => Tree.unsafeGet(state.tree.mostRecentSharedClade(guess, state.answer))) ++ state.hints,
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
  def allPossibleSpecies(state: State): Set[Species] = {
    import state.tree.syntax.*

    val cladeContainingAnswer = closestRevealedClade(state)

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
            clade.childSpeciesTransative
          }
      }

    speciesUnderPossibleClades -- state.guesses
  }

}
