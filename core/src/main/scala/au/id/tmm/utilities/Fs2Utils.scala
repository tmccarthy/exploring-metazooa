package au.id.tmm.utilities

import _root_.fs2.Chunk
import _root_.cats.effect.kernel.Concurrent
import _root_.cats.effect.kernel.Sync

object Fs2Utils {

  def splitBetweenProcessors[F[_] : Concurrent, A, B](sizeHint: Int)(f: Chunk[A] => F[B]): fs2.Pipe[F, A, B] =
    (stream: fs2.Stream[F, A]) => {
      val parNum = (Runtime.getRuntime.availableProcessors - 1) max 1

      val chunkSize = (sizeHint / parNum) + (if ((sizeHint % parNum) == 0) 0 else 1)

      stream
        .chunkN(chunkSize)
        .parEvalMap[F, B](parNum)(f)
    }

  def runEvalAcrossProcessors[F[_] : Concurrent, A, B](chunkSize: Int)(f: Chunk[A] => F[B]): fs2.Pipe[F, A, B] =
    (stream: fs2.Stream[F, A]) => {
      val parNum = (Runtime.getRuntime.availableProcessors - 1) max 1

      stream
        .chunkN(chunkSize)
        .parEvalMap[F, B](parNum)(f)
    }

  def runAcrossProcessors[F[_] : Concurrent : Sync, A, B](chunkSize: Int)(f: Chunk[A] => B): fs2.Pipe[F, A, B] =
    runEvalAcrossProcessors(chunkSize)(chunk => Sync[F].delay(f(chunk)))

}
