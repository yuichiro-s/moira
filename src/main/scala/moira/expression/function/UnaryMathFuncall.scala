package moira.expression.function

import moira.expression.{DimensionInconsistencyException, Expr}
import moira.unit.{CommonDims, SIDim}

// function call
abstract class UnaryMathFuncall(e: Expr) extends Expr {
  def bind(bindings: Bindings) = Sin(e.bind(bindings))
  def unify(x: String, ys: Seq[String]) = Sin(e.unify(x, ys))

  lazy val vars = e.vars

  def dim(varDims: Map[String, SIDim]) = {
    val de = e.dim(varDims)

    if (de == CommonDims.NODIM) {
      CommonDims.NODIM
    } else {
      throw new DimensionInconsistencyException(this,
        s"Argument ${e} is not dimensionless.")
    }
  }
}


