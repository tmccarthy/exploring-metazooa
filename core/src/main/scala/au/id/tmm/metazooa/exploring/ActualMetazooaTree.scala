package au.id.tmm.metazooa.exploring

import java.nio.charset.StandardCharsets

import au.id.tmm.fetch.files.Text
import au.id.tmm.metazooa.exploring.tree.Tree
import cats.effect.IO

object ActualMetazooaTree {

  // TODO this is starting at the root of all life rather than at the Metazoa clade
  val load: IO[Tree] =
    for {
      jsonString <- Text.string(IO(getClass.getResourceAsStream("actual-metazooa-tree.json")), StandardCharsets.UTF_8)
      tree       <- IO.fromEither(io.circe.parser.decode[Tree](jsonString))
    } yield tree

}
