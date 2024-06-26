package au.id.tmm.metazooa.exploring.tree

import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr
import au.id.tmm.metazooa.exploring.tree.Tree.NotInTreeOr.*
import au.id.tmm.utilities.errors.{ExceptionOr, ProductException}
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Invariant, Order}
import io.circe.*
import io.circe.syntax.KeyOps

import scala.annotation.tailrec
import scala.collection.mutable

final case class NcbiId(asLong: Long) extends AnyRef

object NcbiId {
  implicit val order: Order[NcbiId]       = Invariant[Order].imap(Order[Long])(NcbiId.apply)(_.asLong)
  implicit val ordering: Ordering[NcbiId] = order.toOrdering

  implicit val codec: Codec[NcbiId] = Codec.from(
    Decoder[Long].map(NcbiId.apply),
    Encoder[Long].contramap(_.asLong),
  )
}

sealed trait Taxon {
  def ncbiId: NcbiId
}

object Taxon {

  implicit def decoder: Decoder[Taxon] = c =>
    for {
      name      <- c.get[String]("name")
      ncbiId    <- c.get[NcbiId]("ncbiId")
      taxonType <- c.get[String]("type")
      taxon <- taxonType match {
        case "clade" =>
          for {
            children <- c.get[Set[Taxon]]("children")(Decoder.decodeSet[Taxon](decoder))
          } yield Clade(name, ncbiId, children)
        case "species" => Right(Species(name, ncbiId))
        case badType   => Left(DecodingFailure(s"Bad taxon type $badType", c.history))
      }
    } yield taxon

  implicit def encoder: Encoder[Taxon] = {
    case Clade(name, ncbiId, children) =>
      Json.obj(
        "type" := "clade",
        "name" := name,
        "ncbiId" := ncbiId,
        "children" -> Encoder.encodeSet.apply(children),
      )
    case Species(name, ncbiId) =>
      Json.obj(
        "type" := "species",
        "name" := name,
        "ncbiId" := ncbiId,
      )
  }

//  implicit val codec: Codec[Taxon] = Codec.from(
//    Clade.codec.widen[Taxon] or Decoder[Species].widen[Taxon],
//    {
//      case clade: Clade => Encoder[Clade].apply(clade)
//      case species: Species => Encoder[Species].apply(species)
//    }
//  )
}

final case class Clade(
  name: String,
  ncbiId: NcbiId,
  children: Set[Taxon],
) extends Taxon {
  val childSpeciesTransitive: Set[Species] = {
    val builder = Set.newBuilder[Species]

    children.foreach {
      case clade: Clade     => builder.addAll(clade.childSpeciesTransitive)
      case species: Species => builder.addOne(species)
    }

    builder.result()
  }

  def contains(species: Species): Boolean = childSpeciesTransitive.contains(species)

  // Override hashcode to depend on the NcbiID since otherwise this operation becomes extremely expensive
  override def hashCode(): Int = ncbiId.asLong.hashCode()

}

object Clade {
//  val codec: Codec[Clade] = Codec.forProduct3("name", "ncbiId", "children")(Clade.apply)(c => (c.name, c.ncbiId, c.children))
}

final case class Species(
  name: String,
  ncbiId: NcbiId,
) extends Taxon

object Species {
  implicit val codec: Codec[Species] = Codec.forProduct2("name", "ncbiId")(Species.apply)(c => (c.name, c.ncbiId))
}

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

  // Skips the Monophyletic test
  private[tree] def makeUnsafe(cladesRootFirst: List[Clade]) = new Lineage(cladesRootFirst)

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

sealed trait Tree {
  def root: Clade

  def contains(taxon: Taxon): Boolean

  def parentOf(taxon: Taxon): NotInTreeOr[Option[Clade]]

  def treeFrom(clade: Clade): NotInTreeOr[Tree]

  def lineageOf(taxon: Taxon): NotInTreeOr[Lineage] =
    listAllParentsRootFirstFor(taxon).map(Lineage.makeUnsafe)

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

  // TODO really any instance of "lineage.contains" should be replaced with this
  def isAncestor(focus: Taxon, ancestorToTest: Clade): NotInTreeOr[Boolean] =
    iterateAllParentsFor(focus).map { it =>
      it.contains(ancestorToTest)
    }

  def proximityTo(focus: Taxon): Ordering[Taxon] = new Ordering[Taxon] {
    override def compare(left: Taxon, right: Taxon): Int =
      Ordering[Int].compare(
        distance(focus, right).unsafeGet,
        distance(focus, left).unsafeGet,
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
    override def compare(left: Taxon, right: Taxon): Int = {
      for {
        leftUniqueTaxon  <- leastBasalTaxonContainingOnly(left)
        rightUniqueTaxon <- leastBasalTaxonContainingOnly(right)

        leftAllClades  <- listAllCladesRootFirstFor(leftUniqueTaxon)
        rightAllClades <- listAllCladesRootFirstFor(rightUniqueTaxon)
      } yield {
        rightAllClades.size - leftAllClades.size
      }
    }.unsafeGet
  }

  private def iterateAllParentsFor(taxon: Taxon): NotInTreeOr[Iterator[Clade]] =
    if (!this.contains(taxon)) {
      Left(Tree.NotInTreeError(taxon))
    } else {
      Right {
        new Iterator[Clade] {
          private var current: Option[Clade] = parentOf(taxon).unsafeGet

          override def hasNext: Boolean = current.isDefined

          override def next(): Clade = {
            val toReturn = current.get
            current = parentOf(toReturn).unsafeGet
            toReturn
          }
        }
      }
    }

  private def listAllParentsRootFirstFor(taxon: Taxon): NotInTreeOr[List[Clade]] =
    ExceptionOr.catchOnly[Tree.NotInTreeError] {
      val builder = mutable.ListBuffer[Clade]()

      @tailrec
      def go(taxon: Taxon): Unit =
        parentOf(taxon) match {
          case Right(Some(parent)) => {
            builder.prepend(parent)
            go(parent)
          }
          case Right(None) => ()
          case Left(e)     => throw e
        }

      go(taxon)

      builder.result()
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
        if (parent.childSpeciesTransitive.size == 1) {
          leastBasalTaxonContainingOnly(parent).unsafeGet
        } else {
          taxon
        }
    }

  object syntax {
    implicit def taxonOps(taxon: Taxon): Tree.TaxonOps = new Tree.TaxonOps(Tree.this, taxon)
  }

}

object Tree {

  /**
    * Only for creating a brand new tree. If you already have one that contains a parent of the given clade, you
    * get better performance by making a subtree instead.
    */
  def withRoot(root: Clade): Tree = Tree.Total(root)

  final case class NotInTreeError(taxon: Taxon) extends ProductException

  type NotInTreeOr[A] = Either[NotInTreeError, A]

  object NotInTreeOr {
    implicit class Ops[A](notInTreeOr: NotInTreeOr[A]) {
      def unsafeGet: A = notInTreeOr match {
        case Right(a) => a
        case Left(e)  => throw e
      }
    }
  }

  final class TaxonOps private[tree] (tree: Tree, taxon: Taxon) {
    def parent: Option[Clade]             = tree.parentOf(taxon).unsafeGet
    def lineage: Lineage                  = tree.lineageOf(taxon).unsafeGet
    def hasAncestor(that: Clade): Boolean = tree.isAncestor(taxon, that).unsafeGet
  }

  implicit val encoder: Encoder[Tree] = t => Json.obj("root" := (t.root: Taxon))
  implicit val decoder: Decoder[Tree] = Decoder[Taxon].at("root").emap {
    case root: Clade => Right(Tree.Total(root))
    case _: Species  => Left("Expected clade, found species")
  }

  private final case class Total(root: Clade) extends Tree {
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
      if (clade == this.root) {
        Right(this)
      } else if (this.contains(clade)) {
        Right(Tree.SubTree(this, clade))
      } else {
        Left(Tree.NotInTreeError(clade))
      }
  }

  private final case class SubTree(underlying: Tree.Total, root: Clade) extends Tree {
    // TODO replace this with something that performs better
    private val relevantTaxa: Set[Taxon] = {
      val builder = Set.newBuilder[Taxon]

      def go(cladeToProcess: Clade): Unit = {
        builder.addOne(cladeToProcess)
        cladeToProcess.children.foreach {
          case clade: Clade     => go(clade)
          case species: Species => builder.addOne(species)
        }
      }

      go(root)

      builder.result()
    }

    override def contains(taxon: Taxon): Boolean = relevantTaxa.contains(taxon)

    override def parentOf(taxon: Taxon): NotInTreeOr[Option[Clade]] =
      if (!contains(taxon)) {
        Left(NotInTreeError(taxon))
      } else if (taxon == root) {
        Right(None)
      } else {
        underlying.parentOf(taxon)
      }

    override def treeFrom(clade: Clade): NotInTreeOr[Tree] =
      if (!contains(clade)) {
        Left(NotInTreeError(clade))
      } else {
        Right(SubTree(underlying, clade))
      }
  }

}
