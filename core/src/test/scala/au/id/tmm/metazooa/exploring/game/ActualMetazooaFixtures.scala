package au.id.tmm.metazooa.exploring.game

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.tree.{Species, Tree}
import cats.effect.unsafe.IORuntime

object ActualMetazooaFixtures {

  val actualMetazooaTree: Tree = ActualMetazooaTree.load.unsafeRunSync()(IORuntime.global)

  def cleanState(answer: Species): State = State.initial(
    Rules.infinite,
    actualMetazooaTree,
    answer,
  )

  private val speciesByName: Map[String, Species] = actualMetazooaTree.root.childSpeciesTransitive
    .map(s => s.name -> s)
    .toMap

  def speciesWithNameUnsafe(name: String): Species = speciesByName(name)

  val human: Species      = speciesWithNameUnsafe("human")
  val orangutan: Species  = speciesWithNameUnsafe("orangutan")
  val gorilla: Species    = speciesWithNameUnsafe("gorilla")
  val bonobo: Species     = speciesWithNameUnsafe("bonobo")
  val chimpanzee: Species = speciesWithNameUnsafe("chimpanzee")
  val sponge: Species     = speciesWithNameUnsafe("sea sponge")

}
