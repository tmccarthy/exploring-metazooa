package au.id.tmm.metazooa.exploring.buildingtrees

import java.nio.file.Paths

import cats.effect.{IO, IOApp}

object MainBuildingActualMetazooaTree extends IOApp.Simple {

  override def run: IO[Unit] =
    for {
      cachePath <- IO(Paths.get("cache"))
      tree      <- ActualMetazooaTree.make(cachePath)
      _         <- IO.println(tree)
    } yield ()

}
