package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{Move, State}

// TODO model "Perfect" strategies
trait Strategy[F[_]] {

  def proposeMove(state: State.VisibleToPlayer): F[Move]

}
