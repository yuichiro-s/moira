package moira.world

import moira.constraint.Constraint
import moira.constraint.ConstraintType
import moira.constraint.Parameter
import moira.expression.Rel
import moira.expression.RelType
import moira.expression.Expr
import moira.expression.BinOp
import moira.expression.BinOpType
import moira.expression.Var
import moira.expression.Value
import moira.expression.Funcall
import moira.expression.Parser
import moira.unit.SIDim
import moira.unit.PQZero

// Parameter whose definition can be incomplete.
case class ProtoConstraint(id: Int, relStr: String, paramMap: Map[String, ProtoParameter]) {
  val rel: Option[Rel] = Parser.parseRel(relStr)
  val vars: Option[Set[String]] = rel.map(_.vars)
  
  require(vars match {
    case None => true
    case Some(vs) => paramMap.keys.forall(vs.contains(_))
  }, "All variables in /paramMap/ must also appear in /rel/.")

  // Returns variables that do not appear in /paramMap/.
  lazy val disconnectedVariables: Option[Set[String]] = {
    vars.map(_.filter { v => !paramMap.isDefinedAt(v) })
  }

  // Finds inconsistent pair of /Expr/s.
  lazy val isConsistent: Boolean = {
    def getDim(e: Expr): Option[SIDim] = {
      e match {
        case BinOp(op, e1, e2) => {
          val d1 = getDim(e1)
          val d2 = getDim(e2)
          (d1, d2) match {
            case (Some(dim1), Some(dim2)) => {
              op match {
                case BinOpType.Add | BinOpType.Sub => {
                  if (dim1 == dim2) Some(dim1)
                  else None   // e1 and e2 have different dimensions
                }
                case BinOpType.Mul => Some(dim1 * dim2)
                case BinOpType.Div => Some(dim1 / dim2)
              }
            }
            case _ => None  // either /Expr/ doesn't have a valid dimension
          }
        }
        case Var(name) => paramMap.get(name) match {
          case Some(pp) => Some(pp.dim)
          case None => None   // /Var/ is not connected
        }
        case Value(pq) => Some(pq.dim)
        // TODO: case Funcall(_, args) => {}
      }
    }

    rel match {
      case None => false  // relation definition cannot be parsed
      case Some(rel) => {
        (getDim(rel.lhs), getDim(rel.rhs)) match {
          case (Some(dim1), Some(dim2)) => {
            dim1 == dim2 || rel.lhs == Value(PQZero) || rel.rhs == Value(PQZero)
          }
          case _ => false
        }
      }
    }
  }

  lazy val toConstraint: Option[Constraint] = {
    if (disconnectedVariables.size > 0 || !isConsistent) {
      None
    } else {
      // convert related /ProtoParameter/s to /Parameter/s
      val embodiedParamMap: Map[String, Option[Parameter]] = paramMap.mapValues(_.toParameter)

      // if any of the connected /ProtoParameter/s cannot be embodied,
      // it's not possible to convert it into a /ProtoConstraint/.
      if (embodiedParamMap.exists(_._2 == None)) {
        None
      } else {
        // If more than one variables are connected to the same parameter,
        // rename the variables to the same name, since it is not allowed
        // for variables in a /Constraint/ to be connected to the same /Parameter/.
        val pToV: Map[Option[Parameter], Seq[String]] = {
          embodiedParamMap.groupBy(_._2).mapValues(_.keys.toSeq)
        }
        val pf: PartialFunction[(Option[Parameter], Seq[String]), ((String, Parameter), (String, Seq[String]))] = {
          case (Some(param), vs) => {
            val v = vs.head
            (v -> param, (v, vs.tail))
          }
        }
        val (newVarParams, xys) = pToV.collect(pf).unzip match {
          case (a, b) => (a.toMap, b)
        }
        // renamed /rel/
        assert(rel != None, "Relation definition should be able to be parsed.")
        val newRel = xys.foldLeft(rel.get) {
          case (r, (x, ys)) => r.unify(x, ys)
        }

        // current version doesn't take into consideration whether
        // there is an equal sign in the inequality.
        val (t, lhs, rhs) = newRel match {
          case Rel(RelType.Eq,   lhs, rhs) => (ConstraintType.Eq, lhs, rhs)
          case Rel(RelType.Gt,   lhs, rhs) => (ConstraintType.InEq, lhs, rhs)
          case Rel(RelType.GtEq, lhs, rhs) => (ConstraintType.InEq, lhs, rhs)
          case Rel(RelType.Lt,   lhs, rhs) => (ConstraintType.InEq, rhs, lhs)
          case Rel(RelType.LtEq, lhs, rhs) => (ConstraintType.InEq, rhs, lhs)
        }

        Some(Constraint(id, t, lhs, rhs, newVarParams))
      }
    }
  }
}