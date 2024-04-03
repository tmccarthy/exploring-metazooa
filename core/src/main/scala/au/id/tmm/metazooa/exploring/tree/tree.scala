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

  def childSpeciesTransative: Set[Species] = children.flatMap {
    case clade: Clade     => clade.childSpeciesTransative
    case species: Species => Set(species)
  }
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
    if (taxon == root) Right(None) else parentLookup.get(taxon).map(Some(_)).toRight(Tree.NotInTreeError(taxon))

  def treeFrom(clade: Clade): NotInTreeOr[Tree] =
    Either.cond(contains(clade), Tree(clade), Tree.NotInTreeError(clade)) // TODO could be optimised

  def lineageOf(taxon: Taxon): NotInTreeOr[Lineage] =
    listAllParentsRootFirstFor(taxon).map { cladesRootFirst =>
      Lineage(cladesRootFirst) match {
        case Right(lineage) => lineage
        case Left(e)        => throw new AssertionError(e)
      }
    }

  def mostRecentSharedClade(left: Taxon, right: Taxon): NotInTreeOr[Clade] =
    for {
      leftClades  <- listAllCladesRootFirstFor(left)
      rightClades <- listAllCladesRootFirstFor(right)
    } yield (leftClades zip rightClades)
      .findLast { case (leftClade, rightClade) =>
        leftClade == rightClade
      }
      .fold(
        throw new AssertionError("No common shared clade in this tree, despite both taxons being part of tree"),
      ) { case (mostRecentSharedClade, _) =>
        mostRecentSharedClade
      }

  def proximityTo(focus: Taxon): Ordering[Taxon] = new Ordering[Taxon] {
    override def compare(left: Taxon, right: Taxon): Int =
      Ordering[Int].compare(
        unsafeGet(distance(focus, right)),
        unsafeGet(distance(focus, left)),
      )
  }

  def distance(focus: Taxon, test: Taxon): NotInTreeOr[Int] =
    for {
      commonClade       <- mostRecentSharedClade(focus, test)
      allCladesForFocus <- listAllCladesRootFirstFor(focus)
    } yield {
      val lineageUniqueToFocus = allCladesForFocus.takeWhile(_ != commonClade)

      lineageUniqueToFocus.size
    }

  val basality: Ordering[Taxon] = new Ordering[Taxon] {
    override def compare(left: Taxon, right: Taxon): Int = Tree.unsafeGet {
      for {
        leftUniqueTaxon  <- leastBasalTaxonContainingOnly(left)
        rightUniqueTaxon <- leastBasalTaxonContainingOnly(right)

        leftAllClades  <- listAllCladesRootFirstFor(leftUniqueTaxon)
        rightAllClades <- listAllCladesRootFirstFor(rightUniqueTaxon)
      } yield {
        rightAllClades.size - leftAllClades.size
      }
    }
  }

  private def listAllParentsRootFirstFor(taxon: Taxon): NotInTreeOr[List[Clade]] =
    parentOf(taxon).map {
      case Some(parent) => unsafeGet(listAllParentsRootFirstFor(parent)).appended(parent)
      case None         => List.empty
    }

  private def listAllCladesRootFirstFor(taxon: Taxon): NotInTreeOr[List[Clade]] =
    listAllParentsRootFirstFor(taxon).map { parents =>
      taxon match {
        case clade: Clade => parents.appended(clade)
        case _: Species   => parents
      }
    }

  private def leastBasalTaxonContainingOnly(taxon: Taxon): NotInTreeOr[Taxon] =
    parentOf(taxon).map {
      case None => taxon
      case Some(parent) =>
        if (parent.childSpeciesTransative.size == 1) {
          Tree.unsafeGet(leastBasalTaxonContainingOnly(parent))
        } else {
          taxon
        }
    }

  object syntax {
    implicit def taxonOps(taxon: Taxon): Tree.TaxonOps = new Tree.TaxonOps(Tree.this, taxon)
  }

}

object Tree {
  type NotInTreeOr[A] = Either[NotInTreeError, A]
  final case class NotInTreeError(taxon: Taxon) extends ProductException

  def unsafeGet[A](notInTreeOr: NotInTreeOr[A]): A = notInTreeOr match {
    case Right(a) => a
    case Left(e)  => throw new AssertionError(e)
  }

  final class TaxonOps private[tree] (tree: Tree, taxon: Taxon) {
    def parent: Option[Clade] = unsafeGet(tree.parentOf(taxon))
    def lineage: Lineage      = unsafeGet(tree.lineageOf(taxon))
  }
}
//
//final case class Basality private (asInt: Int) extends AnyRef
//
//object Basality {
//  implicit val ordering: Ordering[Basality] = Invariant[Ordering].imap(Ordering.Int)(Basality(_))(_.asInt)
//  implicit val order: Order[Basality] = Invariant[Order].imap(Order[Int])(Basality(_))(_.asInt)
//}
