package moira.expression.function

import moira.expression.{Value, DimensionInconsistencyException, Expr}
import moira.unit.{PhysicalQuantity, CommonDims, SIDim}

sealed abstract class UnaryMathFuncType
object UnaryMathFuncType {
  case object Sin extends UnaryMathFuncType
  case object Cos extends UnaryMathFuncType
  case object Tan extends UnaryMathFuncType

  case object Sinh extends UnaryMathFuncType
  case object Cosh extends UnaryMathFuncType
  case object Tanh extends UnaryMathFuncType

  case object Exp extends UnaryMathFuncType
  case object Log extends UnaryMathFuncType

  case object Abs extends UnaryMathFuncType

  case object Floor extends UnaryMathFuncType
  case object Ceil extends UnaryMathFuncType
}

// function call
case class UnaryMathFuncall(t: UnaryMathFuncType, e: Expr) extends Expr {
  def bind(bindings: Bindings) = UnaryMathFuncall(t, e.bind(bindings))
  def unify(x: String, ys: Seq[String]) = UnaryMathFuncall(t, e.unify(x, ys))

  lazy val vars = e.vars

  lazy val simplified: Expr = {
    e.simplified match {
      case Value(pq) => {
        require(pq.dim == CommonDims.NODIM,
          s"$e is evaluated to $pq and has dimension ${pq.dim}.")

        import UnaryMathFuncType._
        t match {
          case Log if pq.normalized.value <= 0 =>{
            // argument to log is non-positive
            UnaryMathFuncall(t, Value(pq))
          }
          case _ => {
            import Math._
            val f = t match {
              case Sin => sin _
              case Cos => cos _
              case Tan => tan _

              case Sinh => sinh _
              case Cosh => cosh _
              case Tanh => tanh _

              case Exp => exp _
              case Log => log _

              case Abs => abs(_:Double)

              case Floor => floor _
              case Ceil => ceil _
            }

            val ans = f(pq.normalized.value)
            Value(PhysicalQuantity(ans))
          }
        }
      }
      case se => UnaryMathFuncall(t, se)
    }
  }

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


