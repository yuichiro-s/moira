package moira.expression.function

import moira.expression.{Value, Expr, Funcall}
import moira.unit.{PhysicalQuantity, CommonDims}

case class Sin(e: Expr) extends UnaryMathFuncall(e) {
  lazy val simplified: Expr = {
    e.simplified match {
      case Value(pq) => {
        require(pq.dim == CommonDims.NODIM)
        // exponent is rounded down
        val ans = Math.sin(pq.normalized.value)
        Value(PhysicalQuantity(ans))
      }
      case se => Sin(se)
    }
  }
}
