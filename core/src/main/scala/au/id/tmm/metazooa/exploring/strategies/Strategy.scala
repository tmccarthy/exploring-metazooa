package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{Move, State}

trait Strategy[F[_]] {

  def proposeMove(state: State.VisibleToPlayer): F[Move]

}
