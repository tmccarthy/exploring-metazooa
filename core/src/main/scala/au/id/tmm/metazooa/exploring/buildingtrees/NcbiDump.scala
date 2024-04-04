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
    mapCellsFor("nodes.dmp") { cells =>
      val child: NcbiId  = NcbiId(cells.apply(0).trim.toInt)
      val parent: NcbiId = NcbiId(cells.apply(1).trim.toInt)

      child -> parent
    }

  val parentLookup: IO[NcbiDump.ParentLookup] = {
    parents.compile
      .to(Map)
      .map(NcbiDump.ParentLookup)
  }

  val names: fs2.Stream[IO, NcbiDump.Name] =
    mapCellsFor("names.dmp") { cells =>
      NcbiDump.Name(
        ncbiId = NcbiId(cells.apply(0).trim.toInt),
        name = cells.apply(1).trim,
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
    // TODO name type
  )

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
