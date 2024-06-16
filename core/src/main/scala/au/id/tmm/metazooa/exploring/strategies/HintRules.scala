package au.id.tmm.metazooa.exploring.strategies

sealed trait HintRules

object HintRules {
  case object NoHints      extends HintRules
  case object HintsAllowed extends HintRules
}
