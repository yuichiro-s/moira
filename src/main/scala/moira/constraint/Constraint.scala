package moira.constraint

import moira.unit.PhysicalQuantity
import moira.expression.Expr
import moira.expression.BinOp
import moira.expression.BinOpType
import moira.math.NumericalSolver

sealed trait ConstraintType
object ConstraintType {
  case object Eq extends ConstraintType
  case object InEq extends ConstraintType
}

// lhs >= rhs when kind is InEq
// vars: a map from names in the expressions to Parameter instances
case class Constraint(id: Int, cType: ConstraintType, lhs: Expr, rhs: Expr, paramMap: Map[String, Parameter]) {
  val vars: Set[String] = lhs.vars ++ rhs.vars
  val arity: Int = vars.size

  lazy val params: Set[Parameter] = paramMap.values.toSet

  lazy val toExpr: Expr = BinOp(BinOpType.Sub, lhs, rhs).simplified
  lazy val isConstant: Boolean = (arity == 0)
  lazy val isUnary: Boolean = (arity == 1)
  lazy val isEquality: Boolean = (cType == ConstraintType.Eq)

  // make sure all variables in lhs and rhs are defined in params
  require(vars.forall(paramMap.isDefinedAt(_)))

  // make sure no two variables refer to the same parameter
  // because arity is calculated based on the number of variables
  require(params.size == arity)

  // Checks if the constraint is satisfied allowing an error of eps.
  // Returns None if there is still an unbound variable.
  def satisfied(eps: Double=1e-5): Option[Boolean] = {
    (lhs.value, rhs.value) match {
      case (Some(pqL), Some(pqR)) => {
        val l: Double = pqL.normalized.value
        val r: Double = pqR.normalized.value
        cType match {
          case ConstraintType.Eq => Some(NumericalSolver.equal(l, r, eps))
          case ConstraintType.InEq => Some(NumericalSolver.greater(l, r, eps))
        }
      }
      case _ => None
    }
  }

  def bind(bs: Map[Parameter, PhysicalQuantity]): Constraint = {
    // create a map of (variable name -> physical quantity), which can be
    // later applied to lhs and rhs
    val bindings: Map[String, PhysicalQuantity] = (vars.toSeq map { name =>
      bs.get(paramMap(name)) match {
        case Some(pq) => Some(name -> pq)
        case None => None
      }
    }).flatten.toMap

    // parameters in bs are filtered out
    val newParamMap: Map[String, Parameter] = paramMap.filter {
      case (v, p) => !bs.keys.toSeq.contains(p) 
    }

    Constraint(id, cType, lhs.bind(bindings), rhs.bind(bindings), newParamMap)
  }

  // TODO: Check whether it's a polynomial constraint.
  // It's important to know whether it's a polynomial because if so,
  // you know the exact number of solutions.
}

