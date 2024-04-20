package au.id.tmm.metazooa.exploring.tree

import au.id.tmm.utilities.testing.syntax.*
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes.*
import munit.FunSuite

class TreeTest extends FunSuite {
  test("children") {
    assertEquals(pan.children, Set[Taxon](chimpanzee, bonobo))
  }

  test("tree parents") {
    import GreatApes.tree.syntax.*

    assertEquals(human.parent, Some(homo))
  }

  test("create tree from lower") {
    val treeExcludingOrangutans = GreatApes.tree.treeFrom(homininae).get

    assert(treeExcludingOrangutans.contains(human))
    assert(!treeExcludingOrangutans.contains(orangutan))
    assertEquals(treeExcludingOrangutans.parentOf(human).get, Some(homo))
    assertEquals(treeExcludingOrangutans.parentOf(orangutan), Left(Tree.NotInTreeError(orangutan)))
    assertEquals(treeExcludingOrangutans.parentOf(homininae).get, None)
  }

  test("lineage") {
    import GreatApes.tree.syntax.*

    assertEquals(human.lineage, Lineage(hominidae, homininae, homo).get)
  }

  test("clade lineage") {
    import GreatApes.tree.syntax.*

    assertEquals(homo.lineage, Lineage(hominidae, homininae).get)
  }

  test("lineage contains") {
    import GreatApes.tree.syntax.*

    assert(human.lineage.contains(homininae))
  }

  test("most recent shared clade for single species") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(human, human), Right(homo))
  }

  test("most recent shared clade for homo/human") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(human, homo), Right(homo))
  }

  test("most recent shared clade for single clade") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(homo, homo), Right(homo))
  }

  test("most recent shared clade for human/chimp") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(human, chimpanzee), Right(homininae))
  }

  test("most recent shared clade for human/pan") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(human, pan), Right(homininae))
  }

  test("most recent shared clade for chimp/bonobo") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(chimpanzee, bonobo), Right(pan))
  }

  test("most recent shared clade for human/orangutan") {
    assertEquals(GreatApes.tree.mostRecentSharedClade(human, orangutan), Right(GreatApes.tree.root))
  }

  test("basality") {
    implicit val basalityOrdering: Ordering[Taxon] = GreatApes.tree.basality

    import scala.math.Ordering.Implicits.infixOrderingOps

    assert((human: Taxon) < orangutan)
    assert(GreatApes.tree.basality.equiv(human, homo))
    assert(GreatApes.tree.basality.equiv(chimpanzee, bonobo))
    assert((orangutan: Taxon) > gorilla)
  }

  // TODO laws test for basality

  test("human in hominidae") {
    assert(hominidae.contains(human))
  }

  test("human in homo") {
    assert(homo.contains(human))
  }

  test("chimp not in homo") {
    assert(!homo.contains(chimpanzee))
  }
}
