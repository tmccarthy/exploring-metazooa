package au.id.tmm.metazooa.exploring.game

import au.id.tmm.metazooa.exploring.ActualMetazooaTree
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Taxon, Tree}
import au.id.tmm.utilities.errors.GenericException
import cats.effect.unsafe.IORuntime
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*

object ActualMetazooaFixtures {

  val actualMetazooaTree: Tree = ActualMetazooaTree.load.unsafeRunSync()(IORuntime.global)

  private val taxaByName: Map[String, Taxon] = {
    val builder = Map.newBuilder[String, Taxon]

    def go(taxon: Taxon): Unit =
      taxon match {
        case species: Species => builder.addOne(species.name -> species)
        case clade: Clade => {
          builder.addOne(clade.name -> clade)
          clade.children.foreach(go)
        }
      }

    go(actualMetazooaTree.root)

    builder.result()
  }

  def cleanState(answer: Species): State = State.initial(
    Rules.infinite,
    actualMetazooaTree,
    answer,
  )

  def cleanStateVisibleToPlayer: State.VisibleToPlayer = cleanState(human).visibleToPlayer

  def stateRevealedToClade(clade: Clade): State =
    stateRevealedToClade(clade, answer = clade.childSpeciesTransitive.minBy(_.ncbiId))

  def stateRevealedToClade(clade: Clade, answer: Species): State = {
    val initialState = cleanState(answer)

    val hints = initialState.tree.lineageOf(clade).unsafeGet.cladesRootFirst.toSet + clade

    initialState.copy(hints = hints)
  }

  def cladeWithNameUnsafe(name: String): Clade = taxaByName(name) match {
    case species: Species => throw GenericException(species.toString)
    case clade: Clade     => clade
  }

  def speciesWithNameUnsafe(name: String): Species = taxaByName(name) match {
    case species: Species => species
    case clade: Clade     => throw GenericException(clade.toString)
  }

  val metazoa: Clade    = cladeWithNameUnsafe("Metazoa")
  val neognathae: Clade = cladeWithNameUnsafe("Neognathae")
  val percomorphaceae: Clade = cladeWithNameUnsafe("Percomorphaceae")
  val hystricomorpha: Clade = cladeWithNameUnsafe("Hystricomorpha")
  val muroidea: Clade = cladeWithNameUnsafe("Muroidea")

  val human: Species      = speciesWithNameUnsafe("human")
  val orangutan: Species  = speciesWithNameUnsafe("orangutan")
  val gorilla: Species    = speciesWithNameUnsafe("gorilla")
  val bonobo: Species     = speciesWithNameUnsafe("bonobo")
  val chimpanzee: Species = speciesWithNameUnsafe("chimpanzee")
  val sponge: Species     = speciesWithNameUnsafe("sea sponge")

}
