package au.id.tmm.metazooa.exploring.tree

import au.id.tmm.utilities.testing.syntax._
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes
import au.id.tmm.metazooa.exploring.tree.Fixtures.GreatApes.*
import munit.FunSuite

class TreeTest extends FunSuite {
  test("children") {
    assertEquals(pan.children, Set[Taxon](chimpanzee, bonobo))
  }

  test("tree parents") {
    import GreatApes.tree.syntax.*

    assertEquals(human.parent, Some(homininae))
  }

  test("create tree from lower") {
    val treeExcludingOrangutans = GreatApes.tree.treeFrom(homininae).get

    assert(treeExcludingOrangutans.contains(human))
    assert(!treeExcludingOrangutans.contains(orangutan))
    assertEquals(treeExcludingOrangutans.parentOf(human).get, Some(homininae))
    assertEquals(treeExcludingOrangutans.parentOf(orangutan), Left(Tree.NotInTreeError))
    assertEquals(treeExcludingOrangutans.parentOf(homininae).get, None)
  }

  test("lineage") {
    import GreatApes.tree.syntax.*

    assertEquals(human.lineage, Lineage(hominidae, homininae).get)
  }

  test("lineage contains") {
    import GreatApes.tree.syntax.*

    assert(human.lineage.contains(homininae))
  }
}
