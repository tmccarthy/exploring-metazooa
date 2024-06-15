package au.id.tmm.metazooa.exploring.buildingtrees

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import au.id.tmm.collections.{NonEmptyArraySeq, NonEmptySet}
import au.id.tmm.collections.cats.instances.nonEmptyArraySeq.*
import au.id.tmm.fetch.files.Text
import au.id.tmm.metazooa.exploring.tree.*
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.implicits.toNonEmptyTraverseOps
import cats.effect.IO
import cats.implicits.toTraverseOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.matching.Regex

/**
  * Computes the full metazooa tree from a list of species/ncbi IDs stored in a resource
  */
private[buildingtrees] object ComputingFullMetazooaTree {

  def buildFromNcbi(cacheDir: Path): IO[Tree] =
    for {
      actualSpecies <- actualSpeciesRecords
      actualSpecies <-
        IO.fromOption(NonEmptyArraySeq.fromArraySeq(actualSpecies))(GenericException("Empty species list"))

      ncbiDump <- NcbiDump.downloadedInto(cacheDir)

      idPerActualSpeciesRecord <- lookupIdsByName(ncbiDump, actualSpecies.underlying)(_.scientificName)

      species <- actualSpecies.nonEmptyTraverse(r =>
        idPerActualSpeciesRecord.get(r) match {
          case Some(id) => IO.pure(Species(r.commonName, id))
          case None     => IO.raiseError(GenericException(s"No name found for $r"))
        },
      )

      parentLookup <- ncbiDump.parentLookup

      rootUnnamedNode = makeTree(
        species.toNonEmptySet.map(ProcessedSpecies.apply),
        identifyParent = parentLookup.asMap.lift,
      )

      namesForUnnamedNodes <- lookupNamesById(
        ncbiDump,
        rootUnnamedNode.allChildrenRecursive + rootUnnamedNode,
      )(_.ncbiId)

      rootNode <- IO.fromEither(rootUnnamedNode.nameUsing(namesForUnnamedNodes))
    } yield Tree.withRoot(rootNode)

  private def makeTree(
    bottomOfTree: NonEmptySet[ProcessedSpecies],
    identifyParent: NcbiId => Option[NcbiId],
  ): UnnamedClade = {
    val bottomOfTreeLookup: Map[NcbiId, PartiallyProcessedTaxon] =
      bottomOfTree
        .map(t => t.ncbiId -> t)
        .toMap

    val childrenLookup: mutable.Map[NcbiId, mutable.Set[NcbiId]] = mutable.Map()

    @tailrec
    def populateChildrenLookupAndReturnRoot(currentTaxaIds: NonEmptySet[NcbiId]): NcbiId =
      currentTaxaIds.size match {
        case 1 => currentTaxaIds.head
        case _ => {
          val nextTaxaIds: NonEmptySet[NcbiId] = currentTaxaIds.map { (taxonId: NcbiId) =>
            identifyParent(taxonId) match {
              case Some(parentId) => {
                childrenLookup.getOrElseUpdate(parentId, mutable.Set()).addOne(taxonId)
                parentId
              }
              case None => taxonId
            }
          }

          populateChildrenLookupAndReturnRoot(nextTaxaIds)
        }
      }

    def buildPartiallyProcessedTaxonFor(ncbiId: NcbiId): PartiallyProcessedTaxon =
      (bottomOfTreeLookup.get(ncbiId), childrenLookup.get(ncbiId)) match {
        case (Some(processedSpecies: ProcessedSpecies), None) =>
          processedSpecies
        case (Some(_processedSpecies @ _), Some(_children @ _)) =>
          throw new AssertionError("Species with children")
        case (Some(_: UnnamedClade), None) =>
          throw new AssertionError("Clade on the bottom of tree")
        case (None, Some(children)) =>
          UnnamedClade(ncbiId, children.map(buildPartiallyProcessedTaxonFor).toSet)
        case (None, None) =>
          throw new AssertionError("Clade with no children")
      }

    val rootTaxonId = populateChildrenLookupAndReturnRoot(bottomOfTree.map(_.ncbiId))

    buildPartiallyProcessedTaxonFor(rootTaxonId) match {
      case _: ProcessedSpecies => throw GenericException("Bottom of tree is a clade") // TODO bring into types
      case clade: UnnamedClade => clade
    }
  }

  private def lookupNamesById[A](
    ncbiDump: NcbiDump,
    as: Iterable[A],
  )(
    idFrom: A => NcbiId,
  ): IO[Map[NcbiId, String]] = {
    val idsPerA: Map[NcbiId, A] = as.groupBy(idFrom).view.mapValues(_.head).toMap

    ncbiDump.names
      .groupAdjacentBy(_.ncbiId)
      .flatMap { case (id, names) =>
        fs2.Stream.fromOption {
          idsPerA.get(id).map { a =>
            val chosenName = names.apply(names.indexWhere(_.nameType == NcbiDump.Name.Type.ScientificName).getOrElse(0))

            idFrom(a) -> chosenName.name
          }
        }
      }
      .compile
      .to(Map)
  }

  private def lookupIdsByName[A](ncbiDump: NcbiDump, as: Iterable[A])(f: A => String): IO[Map[A, NcbiId]] = {
    val namesPerA: Map[String, A] = as.map(a => f(a) -> a).toMap

    ncbiDump.names
      .mapChunks { chunk =>
        val relevantNames = chunk.filter(name => namesPerA.contains(name.name))

        relevantNames.map { case NcbiDump.Name(ncbiId, name, _) =>
          namesPerA(name) -> ncbiId
        }
      }
      .compile
      .to(Map)
  }

  private val actualSpeciesRecords: IO[ArraySeq[ActualSpeciesRecord]] =
    fs2.io
      .readClassResource[IO, ComputingFullMetazooaTree.type]("actual-metazooa-species.csv")
      .through(Text.lines(StandardCharsets.UTF_8))
      .evalMap {
        case ActualSpeciesRecord.Regex(commonName, scientificName) =>
          IO.pure(ActualSpeciesRecord(commonName, scientificName))
        case badRow => IO.raiseError(GenericException(s"Bad row '${badRow}'"))
      }
      .compile
      .to(ArraySeq)

  private final case class ActualSpeciesRecord(
    commonName: String,
    scientificName: String,
  )

  private object ActualSpeciesRecord {
    val Regex: Regex = """"(.+)","(.+)"""".r
  }

  sealed trait PartiallyProcessedTaxon {
    def ncbiId: NcbiId
  }

  private final case class ProcessedSpecies(asSpecies: Species) extends PartiallyProcessedTaxon {
    override def ncbiId: NcbiId = asSpecies.ncbiId
  }

  private final case class UnnamedClade(
    ncbiId: NcbiId,
    children: Set[PartiallyProcessedTaxon],
  ) extends PartiallyProcessedTaxon {
    def allChildrenRecursive: Set[PartiallyProcessedTaxon] = children ++ children
      .flatMap {
        case clade: UnnamedClade => clade.allChildrenRecursive
        case _: ProcessedSpecies => Set.empty
      }: Set[PartiallyProcessedTaxon]

    def nameUsing(names: Map[NcbiId, String]): ExceptionOr[Clade] =
      for {
        name <- names.get(this.ncbiId).toRight(GenericException(s"No name for ${this.ncbiId}"))
        children <- this.children.toList.traverse[ExceptionOr, Taxon] {
          case ProcessedSpecies(species) => Right(species)
          case clade: UnnamedClade       => clade.nameUsing(names)
        }
      } yield Clade(
        name,
        this.ncbiId,
        children.toSet,
      )
  }

}
