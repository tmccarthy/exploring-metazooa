package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.tree.Species

object NumSpecies {

  def count(species: Set[Species]): NumSpecies = species.size

}
