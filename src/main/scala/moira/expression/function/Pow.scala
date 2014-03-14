package moira.expression.function

import moira.expression.{Value,Expr,Funcall}
import moira.unit.{SIDim,PhysicalQuantity,CommonDims}

case class Pow(e: Expr, ne: Expr) extends Funcall {

  def bind(bindings: Bindings) = Pow(e.bind(bindings), ne.bind(bindings))
  def unify(x: String, ys: Seq[String]) = Pow(e.unify(x, ys), ne.unify(x, ys))

  lazy val vars = e.vars ++ ne.vars

  lazy val simplified: Expr = {
    val se = e.simplified
    val sne = ne.simplified

    (se, sne) match {
      case (Value(pq), Value(npq)) if npq.dim == CommonDims.NODIM => {
        // exponent is rounded down
        Value(pq ** npq.normalized.value.toInt)
      }
      case _ => Pow(se, sne)
    }
  }

  def dim(varDims: Map[String, SIDim]) = {
    val de = e.dim(varDims)
    val dne = ne.dim(varDims)

    (de, dne) match {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(de), Right(dne)) => {
        ne.value match {
          case None => Left(s"There is an unbound variable in ${ne}.")
          case Some(pq) => {
            if (pq.dim == CommonDims.NODIM) {
              Right(de ** pq.normalized.value.toInt)
            } else {
              Left(s"Exponent ${ne} is not dimensionless.")
            }
          }
        }
      }
    }
  }
}
