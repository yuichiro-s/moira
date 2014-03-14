package moira.expression.function

import moira.expression._
import moira.expression.Value
import moira.expression.Var
import moira.unit.SIDim
import moira.unit.PhysicalQuantity

//usage: Integrate($y * $y * $x, $y, 0 m, 100 m,)
case class Integrate(expr: Expr, varExpr: Var, lowerExpr: Expr, upperExpr: Expr) extends Funcall {

  def bind(bindings: Bindings) = Integrate(
    expr.bind(bindings), varExpr, lowerExpr.bind(bindings), upperExpr.bind(bindings))

  def unify(x: String, ys: Seq[String]) = Integrate(
    expr.unify(x, ys), varExpr, lowerExpr.unify(x, ys), upperExpr.unify(x, ys))

  lazy val vars = (expr.vars ++ lowerExpr.vars ++ upperExpr.vars) - varExpr.name

  // Integrates using Simpson's rule.
  lazy val simplified: Expr = {
    val sL = lowerExpr.simplified
    val sU = upperExpr.simplified
    val sE = expr.simplified

    (sL, sU) match {
      case (Value(a), Value(b)) => {
        // unbound variables in /expr/
        val restVars: Set[String] = sE.vars - varExpr.name

        if (restVars.nonEmpty) {
          Integrate(sE, varExpr, Value(a), Value(b))
        } else {
          val N: Int = 100   // number of intervals
          val h: PhysicalQuantity = (b - a) / N
          val ps: Seq[(Int, PhysicalQuantity)] = (1, a) +: ((1 to N-1) map { n: Int =>
            (if (n % 2 == 0) 2 else 4, a + h * n)
          }) :+ (1, b)
          val ans = (ps map {
            case (k, x) => {
              val m = Map(varExpr.name -> x)
              sE.bind(m).value.get * k
            }
          } reduce (_ + _)) * h / 3

          Value(ans)
        }
      }
      case _ => Integrate(sE, varExpr, sL, sU)
    }
  }

  def dim(varDims: Map[String, SIDim]) = {
    val ld = lowerExpr.dim(varDims)
    val ud = upperExpr.dim(varDims)

    if (ld != ud) {
      throw new DimensionInconsistencyException(this,
        s"Lower bound $ld has different dimension from upper bound $ud.")
    } else {
      val newDims = varDims + (varExpr.name -> ld)
      expr.dim(newDims) * ld
    }
  }
}
