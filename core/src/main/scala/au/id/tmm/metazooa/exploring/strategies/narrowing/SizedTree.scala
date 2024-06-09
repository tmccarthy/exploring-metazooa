package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.strategies.NumSpecies
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.Ops
import au.id.tmm.metazooa.exploring.tree.Tree.{NotInTreeError, NotInTreeOr}
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}

// TODO the modelling for this is bad. Should never have to pass it around alongside the game state
private[strategies] sealed trait SizedTree {
  def tree: Tree

  def size: NumSpecies = sizeOfClade(tree.root).unsafeGet

  def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies]

  def subTreeFrom(newRootClade: Clade): NotInTreeOr[SizedTree] =
    if (tree.root == newRootClade) {
      Right(this)
    } else if (!this.tree.contains(newRootClade)) {
      Left(NotInTreeError(newRootClade))
    } else {
      this match {
        case pureTree: SizedTree.Pure =>
          Right(SizedTree.Adjusted(pureTree, newRootClade, Set.empty))
        case SizedTree.Adjusted(underlying, _, excludedSpecies) =>
          Right(SizedTree.Adjusted(underlying, newRootClade, excludedSpecies))
      }
    }

  def excluding(species: Species): SizedTree = this.excluding(Set(species))

  def excluding(species: Set[Species]): SizedTree =
    if (species.isEmpty) {
      this
    } else {
      this match {
        case pureTree: SizedTree.Pure =>
          SizedTree.Adjusted(pureTree, pureTree.tree.root, species)
        case SizedTree.Adjusted(underlying, rootClade, excludedSpecies) =>
          SizedTree.Adjusted(underlying, rootClade, excludedSpecies ++ species)
      }
    }
}

private[strategies] object SizedTree {
  def apply(tree: Tree): SizedTree = Pure(tree)

  private final case class Pure(tree: Tree) extends SizedTree {

    private val sizePerClade: Map[Clade, NumSpecies] = {
      val builder = Map.newBuilder[Clade, NumSpecies]

      def go(clade: Clade): Unit = {
        val sizeOfThisClade = NumSpecies.count(clade.childSpeciesTransitive)
        builder.addOne(clade -> sizeOfThisClade)

        clade.children.foreach {
          case clade: Clade => go(clade)
          case _: Species   => ()
        }
      }

      go(tree.root)

      builder.result()
    }

    def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies] =
      sizePerClade.get(clade).toRight(Tree.NotInTreeError(clade))

  }

  private final case class Adjusted(
    underlying: SizedTree.Pure,
    rootClade: Clade,
    excludedSpecies: Set[Species],
  ) extends SizedTree {
    private val cladesImpactedByExclusions: Set[Clade] = {
      excludedSpecies.flatMap { species =>
        underlying.tree.lineageOf(species).unsafeGet.cladesRootFirst.toSet
      }
    }

    override def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies] =
      if (cladesImpactedByExclusions.contains(clade)) {
        // TODO optimise this line
        val numExcludedSpeciesInClade =
          NumSpecies.count(excludedSpecies) - NumSpecies.count(excludedSpecies -- clade.childSpeciesTransitive)

        underlying.sizeOfClade(clade).map(_ - numExcludedSpeciesInClade)
      } else {
        underlying.sizeOfClade(clade)
      }

    override val tree: Tree = underlying.tree.treeFrom(rootClade).unsafeGet
  }
}
