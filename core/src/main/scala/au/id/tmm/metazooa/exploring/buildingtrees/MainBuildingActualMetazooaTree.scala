package au.id.tmm.metazooa.exploring.buildingtrees

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.{IO, IOApp}
import io.circe.Printer
import io.circe.syntax.EncoderOps

object MainBuildingActualMetazooaTree extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      cachePath <- IO(Paths.get("cache"))
      tree      <- ActualMetazooaTree.make(cachePath)
      treeBytes = tree.asJson.printWith(Printer.spaces2).getBytes(StandardCharsets.UTF_8)
      outputPath <- IO(Files.createTempFile("tree", ".json"))
      _          <- IO(Files.write(outputPath, treeBytes))
      _          <- IO.println(outputPath.toAbsolutePath)
    } yield ()

}
