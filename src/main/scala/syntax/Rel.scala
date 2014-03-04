package moira.syntax

import moira.unit.PhysicalQuantity

// Relation

sealed abstract class RelType
object RelType {
  case object Eq extends RelType
  case object Gt extends RelType
  case object Lt extends RelType
  case object GtEq extends RelType
  case object LtEq extends RelType
}

case class Rel(rType: RelType, lhs: Expr, rhs: Expr) {
  def bind(bindings: Map[String, PhysicalQuantity]): Rel = {
    Rel(rType, lhs.bind(bindings), rhs.bind(bindings))
  }
  lazy val vars = lhs.vars ++ rhs.vars
}

