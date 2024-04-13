package au.id.tmm.metazooa.exploring.strategies

import au.id.tmm.metazooa.exploring.game.{Move, State}

trait Strategy[F[_]] {

  // TODO may be worth having effects or even a probability distribution on this
  def proposeMove(state: State): F[Move]

}
