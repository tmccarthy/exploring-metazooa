package au.id.tmm.metazooa.exploring.strategies
import java.nio.file.Path

import au.id.tmm.digest4s.syntax.*
import au.id.tmm.fetch.cache.{Cache, KVStore, Stores}
import au.id.tmm.metazooa.exploring.game.{Move, Rules, State}
import au.id.tmm.metazooa.exploring.strategies.CachedPerfectStrategy.StateForPerfectPlayer
import au.id.tmm.metazooa.exploring.tree.{Clade, Species, Tree}
import cats.MonadThrow
import cats.effect.{IO, Resource}
import io.circe.parser.decode
import io.circe.syntax.{EncoderOps, KeyOps}
import io.circe.{Encoder, Json}
import spire.ClassTag

/**
  * Assuming the underlying strategy is "perfect" (ie never chooses an answer that is ruled out by visible information)
  */
class CachedPerfectStrategy[F[_], S <: Strategy[F]] private (
  cache: Cache[F, StateForPerfectPlayer, Move, Move],
  underlying: S,
) extends Strategy[F] {
  override def proposeMove(state: State.VisibleToPlayer): F[Move] =
    cache.get(StateForPerfectPlayer.from(state))(underlying.proposeMove(state))
}

object CachedPerfectStrategy {

  def cachingAt[S <: Strategy[IO] : ClassTag](path: Path)(strategy: S): Resource[IO, CachedPerfectStrategy[IO, S]] =
    for {
      stringStore <- Stores.localSqlStringStore(path)
    } yield CachedPerfectStrategy(stringStore, strategy)

  def apply[F[_] : MonadThrow, S <: Strategy[F] : ClassTag](
    store: KVStore[F, String, String, String],
    strategy: S,
  ): CachedPerfectStrategy[F, S] = new CachedPerfectStrategy[F, S](
    cache = Cache(
      store
        .contramapKey[CacheKey](_.encode)
        .contramapKey[StateForPerfectPlayer](s => CacheKey(implicitly, 0, s))
        .contramapValueIn[Move](m => m.asJson.noSpacesSortKeys)
        .evalMapValueOut[Move](s => MonadThrow[F].fromEither(decode[Move](s))),
    ),
    underlying = strategy,
  )

  private final case class CacheKey(
    strategyClass: ClassTag[?],
    strategyVersion: Int = 0,
    state: StateForPerfectPlayer,
  ) {
    def encode: String = Json
      .obj(
        "class" := strategyClass.runtimeClass.getName,
        "version" := strategyVersion,
        "state" := state,
      )
      .noSpacesSortKeys
  }

  private final case class StateForPerfectPlayer(
    rules: Rules,
    tree: Tree,
    closestRevealedClade: Clade,
    guesses: Set[Species],
  )

  private object StateForPerfectPlayer {
    def from(state: State.VisibleToPlayer): StateForPerfectPlayer =
      StateForPerfectPlayer(
        state.rules,
        state.tree,
        state.closestRevealedClade,
        state.guesses,
      )

    implicit val encoder: Encoder[StateForPerfectPlayer] = s =>
      Json.obj(
        "rules" := s.rules,
        "tree" := s.tree.asJson.noSpacesSortKeys.sha256.asBytes.asHexString,
        "closestRevealedClade" := Json.obj(
          "ncbiId" := s.closestRevealedClade.ncbiId,
          "name" := s.closestRevealedClade.name,
        ),
        "guesses" := s.guesses.toList.sortBy(_.ncbiId),
      )

  }
}
