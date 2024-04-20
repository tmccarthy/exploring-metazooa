package au.id.tmm.metazooa.exploring.strategies

import algebra.instances.all.doubleAlgebra
import au.id.tmm.metazooa.exploring.game.{GameUtilities, Move, State}
import au.id.tmm.metazooa.exploring.strategies.PickMostNarrowing.{
  NumRemainingSpecies,
  Scenario,
  logger,
  runAcrossProcessors,
}
import au.id.tmm.metazooa.exploring.tree.Species
import au.id.tmm.utilities.PartialMean
import cats.Id
import cats.effect.{Concurrent, Sync}
import fs2.{Chunk, RaiseThrowable}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.ArraySeq

class PickMostNarrowing[F[_] : Concurrent : Sync] extends Strategy[F] {

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
      .through(runAcrossProcessors(chunkSize = 100)(processScenarios))

    val meanNumRemainingSpeciesPerMove: F[Map[Move, PartialMean[NumRemainingSpecies]]] =
      averageNumRemaingSpeciesPerMove.compile
        .foldMonoid(cats.instances.map.catsKernelStdMonoidForMap)

    Sync[F].map(meanNumRemainingSpeciesPerMove) { lookupAverageRemainingSpecies =>
      lookupAverageRemainingSpecies.keySet.minBy(move => lookupAverageRemainingSpecies(move).map(_.toDouble).toMean)
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

object PickMostNarrowing {
  val logger: Logger = LoggerFactory.getLogger(classOf[PickMostNarrowing[Id]])

  def apply[F[_] : Concurrent : Sync]: PickMostNarrowing[F] = new PickMostNarrowing()

  private final case class Scenario(move: Move, assumedCurrentState: State)

  type NumRemainingSpecies = Int

  // TODO put these somewhere general
  private def runEvalAcrossProcessors[F[_] : Concurrent, A, B](chunkSize: Int)(f: Chunk[A] => F[B]): fs2.Pipe[F, A, B] =
    (stream: fs2.Stream[F, A]) => {
      stream
        .chunkN(chunkSize)
        .parEvalMap[F, B](Runtime.getRuntime.availableProcessors)(f)
    }

  private def runAcrossProcessors[F[_] : Concurrent : Sync, A, B](chunkSize: Int)(f: Chunk[A] => B): fs2.Pipe[F, A, B] =
    runEvalAcrossProcessors(chunkSize)(chunk => Sync[F].delay(f(chunk)))

}
