package moira.world

import moira.constraint.{Constraint,ConstraintType,Parameter}
import moira.expression.{Expr,Rel,RelType,BinOp,BinOpType,Var,Value,Funcall,Parser}
import moira.unit.SIDim
import moira.unit.PQZero

// Parameter whose definition can be incomplete.
case class ProtoConstraint(id: Int = -1, relStr: String = "", paramMap: Map[String, ProtoParameter] = Map()) {
  val rel: Option[Rel] = Parser.parseRel(relStr)
  val vars: Option[Set[String]] = rel.map(_.vars)
  
  vars match {
    case None => assert(paramMap == Map(),
      "/paramMap/ must be None when /relStr/ is not parsable.")
    case Some(vs) => assert(paramMap.keys.forall(vs.contains(_)),
      "All variables in /paramMap/ must also appear in /rel/.")
  }

  // whether it's ready to be converted to a /Constraint/
  lazy val isWellDefined: Boolean = {
    disconnectedVariables == Some(Set.empty) && isConsistent
  }

  // Returns variables that do not appear in /paramMap/.
  lazy val disconnectedVariables: Option[Set[String]] = {
    vars.map(_.filter { v => !paramMap.isDefinedAt(v) })
  }

  // Checks consistency in dimensions.
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
            // It is allowed to compare 0 with any dimensional values
            dim1 == dim2 || rel.lhs == Value(PQZero) || rel.rhs == Value(PQZero)
          }
          case _ => false
        }
      }
    }
  }

  lazy val toConstraint: Option[Constraint] = {
    if (!isWellDefined) {
      None
    } else {
      // convert related /ProtoParameter/s to /Parameter/s

      // partition into parameters with defined values and undefined values
      val (undefs, defs) = paramMap.partition {
        // allow only parameters with undefined value
        case (_, pp) => !pp.value.isDefined
      }

      val embodiedParamMap: Map[String, Option[Parameter]] = undefs.mapValues(_.toParameter)

      // bind defined values to the /rel/
      assert(rel != None, "Relation definition should be able to be parsed.")
      val boundRel: Rel = rel.get.bind(defs.mapValues { pp =>
        // create bindings from already defined parameters
        pp.value match {
          case None => throw new IllegalStateException(
            "/defs/ should contain only /ProtoParameter/s with defined values.")
          case Some(pq) => pq
        }
      })

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
        val renamedRel = xys.foldLeft(boundRel) {
          case (r, (x, ys)) => r.unify(x, ys)
        }

        // TODO: current version doesn't take into consideration whether
        // there is an equal sign in the inequality.
        val (t, lhs, rhs) = renamedRel match {
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
