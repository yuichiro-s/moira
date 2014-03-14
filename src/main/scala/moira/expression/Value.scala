package moira.expression

import moira.unit.{SIDim,PhysicalQuantity}

// value
case class Value(pq: PhysicalQuantity) extends Expr {
  def bind(bindings: Bindings) = this

  lazy val vars = Set[String]()
  lazy val simplified = this

  def dim(varDims: Map[String, SIDim]) = pq.dim

  def unify(x: String, ys: Seq[String]) = this
}
