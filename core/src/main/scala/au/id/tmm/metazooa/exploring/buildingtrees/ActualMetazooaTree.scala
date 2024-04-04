package au.id.tmm.metazooa.exploring.buildingtrees

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import au.id.tmm.collections.syntax.toIterableOps
import au.id.tmm.fetch.files.Text
import au.id.tmm.metazooa.exploring.tree.{NcbiId, Species, Tree}
import au.id.tmm.utilities.errors.{ExceptionOr, GenericException}
import au.id.tmm.utilities.errors.syntax.*
import cats.effect.IO
import cats.implicits.toTraverseOps

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

object ActualMetazooaTree {

  def make(cacheDir: Path): IO[Tree] =
    for {
      actualSpecies <- actualSpeciesRecords
      actualSpeciesPerScientificName = actualSpecies
        .map(speciesRecord => speciesRecord.scientificName -> speciesRecord)
        .toMap

      ncbiDump <- NcbiDump.downloadedInto(cacheDir)

      idPerActualSpeciesRecord <- lookupByName(ncbiDump, actualSpecies)(_.scientificName)

      species <- actualSpecies.traverse(r =>
        idPerActualSpeciesRecord.get(r) match {
          case Some(id) => IO.pure(Species(r.commonName, id))
          case None     => IO.raiseError(GenericException(s"No name found for $r"))
        },
      )

      parentLookup <- ncbiDump.parentLookup

    } yield ???

  private def makeTree[T_NATIVE, T_NODE](
    bottomChildren: Set[T_NODE],
    identifyParent: T_NODE => Option[T_NATIVE],
    makeNode: (T_NATIVE, Set[T_NODE]) => T_NODE,
  ): Option[T_NODE] = {
    val childrenPerParents: Map[T_NATIVE, Set[T_NODE]] =
      bottomChildren
        .groupBy(child => identifyParent(child))
        .collect { case (Some(parent), children) =>
          parent -> children
        }

    val nodes = childrenPerParents.map { case (parent, children) =>
      makeNode(parent, children)
    }.toSet

    nodes.atMostOneOr(()) match {
      case Right(maybeRoot) => maybeRoot
      case Left(_)          => makeTree(nodes, identifyParent, makeNode)
    }
  }

  private def lookupByName[A](ncbiDump: NcbiDump, as: Iterable[A])(f: A => String): IO[Map[A, NcbiId]] = {
    val namesPerA: Map[String, A] = as.map(a => f(a) -> a).toMap

    ncbiDump.names
      .mapChunks { chunk =>
        val relevantNames = chunk.filter(name => namesPerA.contains(name.name))

        relevantNames.map { case NcbiDump.Name(ncbiId, name) =>
          namesPerA(name) -> ncbiId
        }
      }
      .compile
      .to(Map)
  }

  private val actualSpeciesRecords: IO[ArraySeq[ActualSpeciesRecord]] =
    fs2.io
      .readClassLoaderResource[IO]("au.id.tmm.metazooa.exploring.buildingtrees.actual-metazooa-species.csv")
      .through(Text.lines(StandardCharsets.UTF_8))
      .evalMap {
        case ActualSpeciesRecord.Regex(commonName, scientificName) =>
          IO.pure(ActualSpeciesRecord(commonName, scientificName))
        case badRow => IO.raiseError(GenericException(s"Bad row \" $ { badRow } \ ""))
      }
      .compile
      .to(ArraySeq)

  private final case class ActualSpeciesRecord(
    commonName: String,
    scientificName: String,
  )

  private object ActualSpeciesRecord {
    val Regex: Regex = """"(.+)",(.+)"""".r
  }

  private final case class UnnamedClade(
    ncbiId: NcbiId,
    childSpecies: Set[Species],
    childClades: Set[UnnamedClade],
  )

}
