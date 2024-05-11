package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.BruteForceMostNarrowing.{NumRemainingSpecies, Scenario, logger}
import au.id.tmm.metazooa.exploring.tree.Species
import au.id.tmm.utilities.{Fs2Utils, PartialMean}
import cats.Id
import cats.effect.{Concurrent, Sync}
import fs2.{Chunk, RaiseThrowable}
import org.slf4j.{Logger, LoggerFactory}
import spire.math.Rational

import scala.collection.immutable.ArraySeq

class BruteForceMostNarrowing[F[_] : Concurrent : Sync] extends Strategy[F] {

  override def proposeMove(state: State.VisibleToPlayer): F[Move] = {
    val allPossibleSpecies = GameUtilities.allPossibleSpecies(state).to(ArraySeq)

    val scenarios: fs2.Stream[F, Scenario] =
      for {
        assumedAnswer <- fs2.Stream.emits[F, Species](allPossibleSpecies)
        assumedState <- fs2.Stream.fromEither[F](
          state.assumingAnswerIs(assumedAnswer),
        )(RaiseThrowable.fromApplicativeError[F](Sync[F]))
        move <- fs2.Stream
          .emits[F, Species](allPossibleSpecies)
          .map(Move.Guess)
// TODO add the option for a hint. Would need to find a way to weigh hints according to how expensive they are
//          .append {
//            if (assumedState.hintsAvailable) {
//              fs2.Stream.emit(Move.Hint)
//            } else {
//              fs2.Stream.empty
//            }
//          }
      } yield Scenario(move, assumedState)

    val averageNumRemaingSpeciesPerMove: fs2.Stream[F, Map[Move, PartialMean[NumRemainingSpecies]]] = scenarios
      .through(Fs2Utils.runAcrossProcessors(chunkSize = 1000)(processScenarios))

    val meanNumRemainingSpeciesPerMove: F[Map[Move, PartialMean[NumRemainingSpecies]]] =
      averageNumRemaingSpeciesPerMove.compile
        .foldMonoid(cats.instances.map.catsKernelStdMonoidForMap)

    Sync[F].map(meanNumRemainingSpeciesPerMove) { lookupAverageRemainingSpecies =>
      lookupAverageRemainingSpecies.keySet.minBy { move =>
        val meanRemainingSpecies =
          lookupAverageRemainingSpecies(move).map(Rational.apply)(_ + _).toMean // TODO find the semigroup for Rational

        (
          meanRemainingSpecies,
          move match {
            case Move.Guess(species) => species.ncbiId.asLong
            case Move.Hint           => Long.MaxValue
          },
        )
      }
    }
  }

  private def processScenarios(chunk: Chunk[Scenario]): Map[Move, PartialMean[NumRemainingSpecies]] = {
    chunk.head.foreach { case Scenario(move, assumedCurrentState) =>
      // TODO this is gross
      logger.info(
        s"Processing chunk of size ${chunk.size} with first scenario where answer " +
          s"is ${assumedCurrentState.answer} and move is $move",
      )
    }

    chunk.toArraySeq
      .flatMap { scenario =>
        numRemainingSpeciesIn(scenario) match {
          case Right(meanNumRemainingSpecies) => ArraySeq(scenario.move -> meanNumRemainingSpecies)
          case Left(_moveRejected @ _)        => ArraySeq.empty
        }
      }
      .groupMapReduce(_._1)(_._2)(_ |+| _)
  }

  private def numRemainingSpeciesIn(
    scenario: Scenario,
  ): Either[Move.RejectionReason, PartialMean[NumRemainingSpecies]] =
    scenario.assumedCurrentState
      .applyMove(scenario.move)
      .map(s => GameUtilities.allPossibleSpecies(s.visibleToPlayer))
      .map(remainingSpecies => PartialMean.singleValue(remainingSpecies.size))

}

object BruteForceMostNarrowing {
  val logger: Logger = LoggerFactory.getLogger(classOf[BruteForceMostNarrowing[Id]])

  def apply[F[_] : Concurrent : Sync]: BruteForceMostNarrowing[F] = new BruteForceMostNarrowing()

  private final case class Scenario(move: Move, assumedCurrentState: State)

  type NumRemainingSpecies = Int

}
