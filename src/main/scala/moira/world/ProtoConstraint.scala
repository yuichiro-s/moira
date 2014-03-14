package moira.world

import moira.constraint.{Constraint,ConstraintType,Parameter}
import moira.expression.{Expr,Rel,RelType,BinOp,BinOpType,Var,Funcall,Parser}
import moira.unit.SIDim
import moira.unit.PQZero
import moira.expression.Value

// Parameter whose definition can be incomplete.
case class ProtoConstraint(id: Int = -1, relStr: String = "", paramMap: Map[String, ProtoParameter] = Map()) {
  val rel: Option[Rel] = Parser.parseRel(relStr)

  lazy val (boundRel, embodiedParamMap): (Option[Rel], Map[String, Option[Parameter]]) = {
    // partition into parameters with defined values and undefined values
    val (defs, undefs) = paramMap.partition {
      // allow only parameters with undefined value
      case (_, pp) => pp.value.isDefined
    }

    // bind defined values to the /rel/
    val a: Option[Rel] = rel.map {
      _.bind(defs.mapValues { pp =>
      // create bindings from already defined parameters
        pp.value match {
          case None => throw new IllegalStateException(
            "/defs/ should contain only /ProtoParameter/s with defined values.")
          case Some(pq) => pq
        }
      })
    }

    val b: Map[String, Option[Parameter]] = undefs.mapValues(_.toParameter)

    (a, b)
  }

  val vars: Option[Set[String]] = rel.map(_.vars)
  
  vars match {
    case None => assert(paramMap == Map(),
      "/paramMap/ must be None when /relStr/ is not parsable.")
    case Some(vs) => assert(paramMap.keys.forall(vs.contains(_)),
      "There is a variable in %s which doesn't appear in %s.".format(paramMap, vs))
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
    boundRel match {
      case None => false  // relation definition cannot be parsed
      case Some(rel) => {
        val dimMap = paramMap.mapValues(_.dim)
        (rel.lhs.dim(dimMap), rel.rhs.dim(dimMap)) match {
          case (Left(e), _) => { println(e); false }
          case (_, Left(e)) => { println(e); false }
          case (Right(dim1), Right(dim2)) => {
            // It is allowed to compare 0 with any dimensional values
            dim1 == dim2 || rel.lhs == Value(PQZero) || rel.rhs == Value(PQZero)
          }
        }
      }
    }
  }

  lazy val toConstraint: Option[Constraint] = {
    if (!isWellDefined) {
      None
    } else {
      // convert related /ProtoParameter/s to /Parameter/s

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
        val renamedRel = xys.foldLeft(boundRel.get) {
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

object ProtoConstraint {
  def fromXML(source: xml.Node, pMap: Map[Int, ProtoParameter]): ProtoConstraint = {
    val id = (source \ "id").text.toInt
    val relStr = (source \ "rel").text
    val paramMap = ((source \\ "var") collect {
      case n if !(n \\ "pid").isEmpty => {
        val nn = n \\ "name"
        val pn = n \\ "pid"
        nn.text -> pMap(pn.text.toInt)
      }
    }).toMap

    ProtoConstraint(id, relStr, paramMap)
  }
}
