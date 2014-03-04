package moira.expression

import moira.unit.SIDim
import moira.unit.SIUnit
import moira.unit.PhysicalQuantity

// Expression

sealed trait Expr {
  type Bindings = Map[String, PhysicalQuantity]

  def bind(bindings: Bindings): Expr
  val vars: Set[String]
  val simplified: Expr

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

sealed trait BinOpType
object BinOpType {
  case object Add extends BinOpType
  case object Sub extends BinOpType
  case object Mul extends BinOpType
  case object Div extends BinOpType
  case object Pow extends BinOpType
}

// binary operation
case class BinOp(op: BinOpType, e1: Expr, e2: Expr) extends Expr {
  def bind(bindings: Bindings) = {
    BinOp(op, e1.bind(bindings), e2.bind(bindings))
  }

  def unify(x: String, ys: Seq[String]): Expr = {
    BinOp(op, e1.unify(x, ys), e2.unify(x, ys))
  }

  lazy val vars = e1.vars ++ e2.vars

  lazy val simplified: Expr = {
    val s1 = e1.simplified
    val s2 = e2.simplified
    (s1, s2) match {
      case (Value(pq1), Value(pq2)) => {
        val ans: PhysicalQuantity = op match {
          case BinOpType.Add => pq1 + pq2
          case BinOpType.Sub => pq1 - pq2
          case BinOpType.Mul => pq1 * pq2
          case BinOpType.Div => pq1 / pq2
        }
        Value(ans)
      }
      case _ => BinOp(op, s1, s2)
    }
  }
}

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

  def unify(x: String, ys: Seq[String]): Expr = {
    if (ys.contains(name)) {
      Var(x)
    } else {
      this
    }
  }
}

// value
case class Value(pq: PhysicalQuantity) extends Expr {
  def bind(bindings: Bindings) = Value(pq)

  lazy val vars = Set[String]()
  lazy val simplified = this

  def unify(x: String, ys: Seq[String]) = this
}

// function call
case class Funcall(name: String, args: Seq[Expr]) extends Expr {
  def bind(bindings: Bindings) = {
    Funcall(name, args.map(_.bind(bindings)))
  }

  def unify(x: String, ys: Seq[String]): Expr = {
    Funcall(name, args.map(_.unify(x, ys)))
  }

  lazy val vars = args.foldLeft(Set[String]()) { (vs, e) => vs ++ e.vars }
  lazy val simplified = Funcall(name, args.map(_.simplified))

}
