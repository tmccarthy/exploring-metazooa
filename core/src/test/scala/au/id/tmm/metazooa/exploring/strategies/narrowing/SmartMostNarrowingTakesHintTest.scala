package au.id.tmm.metazooa.exploring.strategies.narrowing

import au.id.tmm.metazooa.exploring.game.{ActualMetazooaFixtures, Move}
import au.id.tmm.metazooa.exploring.game.ActualMetazooaFixtures.{percomorphaceae, speciesWithNameUnsafe}
import au.id.tmm.metazooa.exploring.game.Move.Hint
import au.id.tmm.metazooa.exploring.strategies.HintRules
import cats.effect.IO
import munit.CatsEffectSuite

class SmartMostNarrowingTakesHintTest extends CatsEffectSuite {

  test("take a hint at Percomorphaceae") {
    for {
      sut <- SmartMostNarrowing[IO](hintRules = HintRules.HintsAllowed)

      state = ActualMetazooaFixtures.stateRevealedToClade(percomorphaceae, answer = speciesWithNameUnsafe("bass"))
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
