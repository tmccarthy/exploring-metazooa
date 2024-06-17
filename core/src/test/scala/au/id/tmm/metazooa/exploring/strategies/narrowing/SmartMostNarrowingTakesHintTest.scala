package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, Move}
import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures.{neognathae, speciesWithNameUnsafe}
import au.id.tmm.metazooa.exploring.game.Move.Hint
import au.id.tmm.metazooa.exploring.strategies.HintRules
import cats.effect.IO
import munit.CatsEffectSuite

class SmartMostNarrowingTakesHintTest extends CatsEffectSuite {

  test("take a hint at birds") {
    for {
      sut <- SmartMostNarrowing[IO](hintRules = HintRules.HintsAllowed)

      state = ActualMetazooaFixtures.stateRevealedToClade(neognathae, answer = speciesWithNameUnsafe("raven"))
    } yield assertIO(sut.proposeMove(state.visibleToPlayer), Hint)
  }

  test("not take a hint at krill") {
    for {
      sut <- SmartMostNarrowing[IO](hintRules = HintRules.HintsAllowed)

      state = ActualMetazooaFixtures
        .cleanState(speciesWithNameUnsafe("krill"))
        .copy(guesses =
          Set(
            speciesWithNameUnsafe("mink"),
            speciesWithNameUnsafe("hornet"),
            speciesWithNameUnsafe("lobster"),
          ),
        )
    } yield assertIO(sut.proposeMove(state.visibleToPlayer), Move.Guess(speciesWithNameUnsafe("krill")))
  }

}
