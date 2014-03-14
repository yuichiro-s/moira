package moira.expression

import moira.unit.SIDim

// variable
case class Var(name: String) extends Expr {
  def bind(bindings: Bindings) = {
    bindings.get(name) match {
      case Some(pq) => Value(pq)
      case None => Var(name)
    }
  }

  lazy val vars = Set(name)
  lazy val simplified = this

  def dim(varDims: Map[String, SIDim]) = {
    varDims.get(name) match {
      case None => throw new DimensionInconsistencyException(this,
        s"Dimension is unknown.")
      case Some(d) => d
    }
  }

  def unify(x: String, ys: Seq[String]): Expr = {
    if (ys.contains(name)) {
      Var(x)
    } else {
      this
    }
  }
}
