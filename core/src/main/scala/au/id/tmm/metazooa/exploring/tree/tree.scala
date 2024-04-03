package au.id.tmm.metazooa.exploring.tree

import au.id.tmm.metazooa.exploring.tree.Tree.{NotInTreeOr, unsafeGet}
import au.id.tmm.utilities.errors.ProductException
import cats.syntax.functor.*
import cats.syntax.traverse.*

final case class NcbiId(asLong: Long) extends AnyRef

sealed trait Taxon {
  def ncbiId: NcbiId
}

final case class Clade(
  name: String,
  ncbiId: NcbiId,
  children: Set[Taxon],
) extends Taxon {
  def asTree: Tree = Tree(this)
}

final case class Species(
  name: String,
  ncbiId: NcbiId,
) extends Taxon

final case class Lineage private (
  cladesRootFirst: List[Clade],
) extends AnyVal {
  def contains(clade: Clade): Boolean = cladesRootFirst.contains(clade)

  override def toString: String = cladesRootFirst match {
    case clades @ _ :: _ => clades.map(_.name).mkString("[", "->", "]")
    case Nil             => "[empty lineage]"
  }
}

object Lineage {
  val empty: Lineage = new Lineage(List.empty)

  def apply(cladesRootFirst: List[Clade]): Either[NotMonophyleticError, Lineage] = {
    val errorOrUnit = cladesRootFirst
      .sliding(2)
      .toList // TODO find a cats way to do this
      .traverse {
        case Seq(parent, child) => {
          if (parent.children.contains(child)) {
            Right(())
          } else {
            Left(NotMonophyleticError(parent, child, cladesRootFirst))
          }
        }
        case _ => Right(())
      }

    errorOrUnit.as(new Lineage(cladesRootFirst))
  }

  def apply(cladesRootFirst: Clade*): Either[NotMonophyleticError, Lineage] = apply(cladesRootFirst.toList)

  final case class NotMonophyleticError(
    problematicParent: Clade,
    problematicChild: Clade,
    allClades: List[Clade],
  ) extends RuntimeException {
    override def getMessage: String = {
      val wholeLineageString = allClades.map(_.name).mkString("[", " -> ", "]")

      s"Clade ${problematicParent.name} is not parent for ${problematicChild.name}. " +
        s"Whole lineage was $wholeLineageString."
    }
  }
}

final case class Tree private (
  root: Clade,
) {
  private val parentLookup: Map[Taxon, Clade] = {
    def makeParentLookupFor(root: Clade): Map[Taxon, Clade] = {
      val lookupBuilder = Map.newBuilder[Taxon, Clade]
      root.children.foreach { child =>
        lookupBuilder.addOne((child, root))

        child match {
          case childClade: Clade => lookupBuilder.addAll(makeParentLookupFor(childClade))
          case _: Species        => ()
        }
      }

      lookupBuilder.result()
    }

    makeParentLookupFor(root)
  }

  def contains(taxon: Taxon): Boolean = parentLookup.contains(taxon)

  def parentOf(taxon: Taxon): NotInTreeOr[Option[Clade]] =
    if (taxon == root) Right(None) else parentLookup.get(taxon).map(Some(_)).toRight(Tree.NotInTreeError)
  def treeFrom(clade: Clade): NotInTreeOr[Tree] =
    Either.cond(contains(clade), Tree(clade), Tree.NotInTreeError) // TODO could be optimised
  def lineageOf(taxon: Taxon): NotInTreeOr[Lineage] =
    parentOf(taxon).map {
      case Some(parent) =>
        Lineage(unsafeGet(lineageOf(parent)).cladesRootFirst :+ parent) match {
          case Right(lineage) => lineage
          case Left(e)        => throw new AssertionError(e)
        }
      case None => Lineage.empty
    }

  object syntax {
    implicit def taxonOps(taxon: Taxon): Tree.TaxonOps = new Tree.TaxonOps(Tree.this, taxon)
  }

}

object Tree {
  type NotInTreeOr[A] = Either[NotInTreeError.type, A]
  case object NotInTreeError extends ProductException

  private[tree] def unsafeGet[A](notInTreeOr: NotInTreeOr[A]): A = notInTreeOr match {
    case Right(a) => a
    case Left(e)  => throw new AssertionError(e)
  }

  final class TaxonOps private[tree] (tree: Tree, taxon: Taxon) {
    def parent: Option[Clade] = unsafeGet(tree.parentOf(taxon))
    def lineage: Lineage      = unsafeGet(tree.lineageOf(taxon))
  }
}
