package au.id.tmm.metazooa.exploring.buildingtrees

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipFile

import au.id.tmm.fetch.files.{Downloading, Text}
import au.id.tmm.metazooa.exploring.tree.NcbiId
import au.id.tmm.utilities.errors.ExceptionOr
import cats.effect.IO

class NcbiDump private (dumpZipPath: Path) {

  private def linesFor(entryName: String): fs2.Stream[IO, String] =
    Text.lines(
      makeInputStream = for {
        zipFile <- IO(new ZipFile(dumpZipPath.toFile))
        entry   <- IO(zipFile.getEntry(entryName))
        stream  <- IO(zipFile.getInputStream(entry))
      } yield stream,
      charset = StandardCharsets.UTF_8,
      chunkSize = 500,
    )

  private def mapCellsFor[A](entryName: String)(unsafeMap: Array[String] => A): fs2.Stream[IO, A] =
    linesFor(entryName)
      .evalMap { line =>
        val cells = line.split('|')

        IO.fromEither(ExceptionOr.catchIn(unsafeMap(cells)))
      }

  val parents: fs2.Stream[IO, (NcbiId, NcbiId)] =
    linesFor("nodes.dmp")
      .flatMap { line =>
        val cells = line.split('|')

        val child: NcbiId  = NcbiId(cells.apply(0).trim.toLong)
        val parent: NcbiId = NcbiId(cells.apply(1).trim.toLong)

        if (child == parent) {
          fs2.Stream.empty
        } else {
          fs2.Stream(child -> parent)
        }
      }

  val parentLookup: IO[NcbiDump.ParentLookup] = {
    parents.compile
      .to(Map)
      .map(NcbiDump.ParentLookup)
  }

  val names: fs2.Stream[IO, NcbiDump.Name] =
    mapCellsFor("names.dmp") { cells =>
      NcbiDump.Name(
        ncbiId = NcbiId(cells.apply(0).trim.toLong),
        name = cells.apply(1).trim,
        nameType = NcbiDump.Name.Type.parse(cells.apply(3).trim),
      )
    }

}

object NcbiDump {

  final case class ParentLookup(asMap: Map[NcbiId, NcbiId]) {
    def parentOf(child: NcbiId): Option[NcbiId] = asMap.get(child)
  }

  final case class Name(
    ncbiId: NcbiId,
    name: String,
    // TODO unique name
    nameType: Name.Type,
  )

  object Name {
    sealed abstract class Type(val asString: String)

    object Type {
      case object Acronym                              extends Type("acronym")
      case object Authority                            extends Type("authority")
      case object BlastName                            extends Type("blast name")
      case object CommonName                           extends Type("common name")
      case object EquivalentName                       extends Type("equivalent name")
      case object GenbankAcronym                       extends Type("genbank acronym")
      case object GenbankCommonName                    extends Type("genbank common name")
      case object Inpart                               extends Type("in-part")
      case object Includes                             extends Type("includes")
      case object ScientificName                       extends Type("scientific name")
      case object Synonym                              extends Type("synonym")
      case object TypeMaterial                         extends Type("type material")
      final case class Other(unrecognisedCode: String) extends Type(unrecognisedCode)

      def parse(code: String): Type = code match {
        case Acronym.asString           => Acronym
        case Authority.asString         => Authority
        case BlastName.asString         => BlastName
        case CommonName.asString        => CommonName
        case EquivalentName.asString    => EquivalentName
        case GenbankAcronym.asString    => GenbankAcronym
        case GenbankCommonName.asString => GenbankCommonName
        case Inpart.asString            => Inpart
        case Includes.asString          => Includes
        case ScientificName.asString    => ScientificName
        case Synonym.asString           => Synonym
        case TypeMaterial.asString      => TypeMaterial
        case unrecognisedCode           => Other(unrecognisedCode)
      }
    }
  }

  def downloadedInto(directory: Path, replaceExisting: Boolean = false): IO[NcbiDump] =
    for {
      dumpZipPath <- IO(directory.resolve("taxdump.zip"))
      _ <- Downloading.inputStreamToPath(
        dumpZipPath,
        replaceExisting,
        for {
          uri    <- IO(URI.create("https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdmp.zip"))
          stream <- IO(uri.toURL.openStream())
        } yield stream,
      )
    } yield new NcbiDump(dumpZipPath)

}
