package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.Ops
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}

private sealed trait SizedTree {
  def tree: Tree

  def size: NumSpecies = sizeOfClade(tree.root).unsafeGet

  def sizeOfClade(clade: Clade): NotInTreeOr[NumSpecies]

  def excluding(species: Species): SizedTree = this.excluding(Set(species))

  def excluding(species: Set[Species]): SizedTree = if (species.isEmpty) {
    this
  } else {
    this match {
      case pureTree: SizedTree.Pure =>
        SizedTree.WithExclusion(pureTree, species)
      case SizedTree.WithExclusion(underlying, excludedSpecies) =>
        SizedTree.WithExclusion(underlying, excludedSpecies ++ species)
    }
  }
}

private object SizedTree {
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

  private final case class WithExclusion(underlying: SizedTree.Pure, excludedSpecies: Set[Species]) extends SizedTree {
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

    override def tree: Tree = underlying.tree
  }
}
