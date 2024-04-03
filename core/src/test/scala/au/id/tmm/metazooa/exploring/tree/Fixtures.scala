package au.id.tmm.metazooa.exploring.tree

object Fixtures {

  object GreatApes {
    val human: Species = Species(
      name = "Human",
      ncbiId = NcbiId(770315),
    )

    val orangutan: Species = Species(
      name = "Sumatran orangutan",
      ncbiId = NcbiId(9601),
    )

    val gorilla: Species = Species(
      name = "Gorilla",
      ncbiId = NcbiId(417965),
    )
    val chimpanzee: Species = Species(
      name = "Chimpanzee",
      ncbiId = NcbiId(417950),
    )
    val bonobo: Species = Species(
      name = "Bonobo",
      ncbiId = NcbiId(158484),
    )
    val pan: Clade = Clade(
      name = "Pan",
      ncbiId = NcbiId(417957),
      children = Set(
        chimpanzee,
        bonobo,
      ),
    )
    val homininae: Clade = Clade(
      name = "Homininae",
      ncbiId = NcbiId(312031),
      children = Set(
        gorilla,
        pan,
        human,
      ),
    )
    val hominidae: Clade = Clade(
      name = "Hominidae",
      ncbiId = NcbiId(770311),
      children = Set(
        orangutan,
        homininae,
      ),
    )
    val tree: Tree = Tree(
      root = hominidae,
    )

  }

}
