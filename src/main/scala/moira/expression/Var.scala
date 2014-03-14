package moira.expression

import moira.constraint.solver.ConstraintSolver.Bindings
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
      case None => Left("Dimension of variable '%s' is unknown.".format(name))
      case Some(d) => Right(d)
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
