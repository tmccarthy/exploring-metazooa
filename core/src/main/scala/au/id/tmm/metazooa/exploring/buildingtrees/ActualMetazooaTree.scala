package au.id.tmm.metazooa.exploring.buildingtrees

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.fetch.files.Text
import au.id.tmm.metazooa.exploring.tree.*
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import cats.effect.IO
import cats.implicits.toTraverseOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

object ActualMetazooaTree {

  def make(cacheDir: Path): IO[Tree] =
    for {
      actualSpecies <- actualSpeciesRecords

      ncbiDump <- NcbiDump.downloadedInto(cacheDir)

      idPerActualSpeciesRecord <- lookupIdsByName(ncbiDump, actualSpecies)(_.scientificName)

      species <- actualSpecies.traverse(r =>
        idPerActualSpeciesRecord.get(r) match {
          case Some(id) => IO.pure(Species(r.commonName, id))
          case None     => IO.raiseError(GenericException(s"No name found for $r"))
        },
      )

      parentLookup <- ncbiDump.parentLookup

      maybeRootUnnamedNode = makeTree(
        species.toSet.map(ProcessedSpecies.apply),
        identifyParent = {
          case ProcessedSpecies(species) => parentLookup.parentOf(species.ncbiId)
          case UnnamedClade(ncbiId, _)   => parentLookup.parentOf(ncbiId)
        },
      )

      rootUnnamedNode <- IO.fromEither(maybeRootUnnamedNode.toRight(GenericException("No root node")))

      namesForUnnamedNodes <- lookupNamesById(ncbiDump, rootUnnamedNode.allChildrenRecursive)(_.ncbiId)

      rootNode <- IO.fromEither(rootUnnamedNode.nameUsing(namesForUnnamedNodes))
    } yield Tree(rootNode)

  @tailrec
  private def makeTree(
    bottomOfTree: Set[PartiallyProcessedTaxon],
    identifyParent: PartiallyProcessedTaxon => Option[NcbiId],
  ): Option[UnnamedClade] = {
    val childrenPerParentId: Map[NcbiId, Set[PartiallyProcessedTaxon]] =
      bottomOfTree
        .groupBy(child => identifyParent(child))
        .collect { case (Some(parent), children) =>
          parent -> children
        }

    val clades = childrenPerParentId.map { case (id, children) =>
      UnnamedClade(
        id,
        children,
      )
    }.toList

    clades.atMostOneOr(()) match {
      case Right(maybeRoot) => maybeRoot
      case Left(_)          => makeTree((clades: List[PartiallyProcessedTaxon]).toSet, identifyParent)
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
      .readClassResource[IO, ActualMetazooaTree.type]("actual-metazooa-species.csv")
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
