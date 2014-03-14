package moira.expression

import moira.unit.SIDim
import moira.unit.SIUnit
import moira.unit.PhysicalQuantity

class DimensionInconsistencyException(e: Expr, msg: String) extends RuntimeException(s"$e: $msg")

// Expression
trait Expr {
  type Bindings = Map[String, PhysicalQuantity]

  def bind(bindings: Bindings): Expr
  val vars: Set[String]
  val simplified: Expr

  // Calculates the dimension of the expression.
  // In case of dimension inconsistency, reports error.
  def dim(varDims: Map[String, SIDim]): SIDim

  // Renames all /Var/s with any of the names in /ys/ to /x/.
  def unify(x: String, ys: Seq[String]): Expr

  // Evaluates the /Expr/.
  // If there is an unbound variable, returns None.
  lazy val value: Option[PhysicalQuantity] = {
    simplified match {
      case Value(pq) => Some(pq)
      case _ => None
    }
  }

  // Converts to a unary function.
  // Only applicable to a unary expression.
  def toUnaryFunc(v: String, dim: SIDim): Double => Double = {
    require(vars.size == 1)
    require(v == vars.toSeq.head)

    val unit = SIUnit(1.0, dim) // unit assumed for the argument value
    x: Double => {
      val bs = Map(v -> PhysicalQuantity(x, unit))
      val ans = bind(bs).value.get // assumes that there are no unbound variables
      ans.normalized.value
    }
  }

  // Converts to a multi-variable function.
  // vs: Sequence of pairs of variable name and dimension.
  //     None indicates that the variable at the position doesn't appear
  //     in this equation.
  def toFunc(vs: Seq[Option[(String, SIDim)]]): Seq[Double] => Double = {
    // look at toUnaryFunc
    val units: Seq[Option[SIUnit]] = vs map {
      case Some((_, dim)) => Some(SIUnit(1.0, dim))
      case None => None
    }
    xs: Seq[Double] => {
      val pqs: Seq[Option[PhysicalQuantity]] = xs.zip(units) map {
        case (x, Some(unit)) => Some(PhysicalQuantity(x, unit))
        case (_, None) => None
      }
      val bs = vs.zip(pqs) collect {
        case (Some((name, _)), Some(pq)) => name -> pq
      }
      val ans = bind(bs.toMap).value.get // assumes that there are no unbound variables
      ans.normalized.value
    }
  }
}

