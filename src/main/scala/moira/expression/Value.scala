package moira.expression

import moira.unit.PhysicalQuantity

// value
case class Value(pq: PhysicalQuantity) extends Expr {
  def bind(bindings: Bindings) = Value(pq)

  lazy val vars = Set[String]()
  lazy val simplified = this

  def unify(x: String, ys: Seq[String]) = this
}
