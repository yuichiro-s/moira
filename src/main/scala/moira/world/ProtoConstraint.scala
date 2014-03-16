package moira.world

import moira.constraint.{Constraint,ConstraintType,Parameter}
import moira.expression.{Expr,Rel,RelType,BinOp,BinOpType,Var,Funcall,Parser}
import moira.unit.SIDim
import moira.unit.PQZero
import moira.expression.Value

class InsufficientConstraintConfigException(pc: ProtoConstraint, msg: String) extends RuntimeException(s"[id=${pc.id}]${pc.relStr}: $msg")

// Parameter whose definition can be incomplete.
case class ProtoConstraint(id: Int = -1, relStr: String = "", paramMap: Map[String, Int] = Map()) {
  lazy val rel: Option[Rel] = Parser.parseRel(relStr)
  lazy val vars: Option[Set[String]] = rel.map(_.vars)
  
  vars match {
    case None => assert(paramMap == Map(),
      "/paramMap/ must be None when /relStr/ is not parsable.")
    case Some(vs) => assert(paramMap.keys.forall(vs.contains),
      s"There is a variable in $paramMap which doesn't appear in $vs.")
  }

  def toConstraint(pMap: Map[Int, ProtoParameter]): Constraint = {
    rel match {
      case None => throw new InsufficientConstraintConfigException(this,
        s"$relStr is not a complete definition.")
      case Some(rel) => {
        // Returns variables that do not appear in /paramMap/.
        val disconnectedVariables: Set[String] = rel.vars.filter { v => !paramMap.isDefinedAt(v) }

        if (disconnectedVariables.nonEmpty) {
          throw new InsufficientConstraintConfigException(this,
            s"Unbound variable(s): $disconnectedVariables")
        }

        val ppMap: Map[String, ProtoParameter] = paramMap mapValues { id =>
          pMap.get(id) match {
            case Some(pp) => pp
            case None => throw new IllegalArgumentException(
              s"id=$id is not defined in $paramMap.")
          }
        }

        // partition into parameters with defined values and undefined values
        val (defs, undefs) = ppMap.partition {
          // allow only parameters with undefined value
          case (_, pp) => pp.value.isDefined
        }

        // bind defined values to /rel/
        val boundRel: Rel = rel.bind(defs.mapValues { pp =>
          // create bindings from already defined parameters
          pp.value match {
            case None => throw new IllegalStateException(
            "/defs/ should contain only /ProtoParameter/s with defined values.")
            case Some(pq) => pq
          }
        })

        val embodiedParamMap: Map[String, Parameter] = undefs.mapValues(_.toParameter)

        // check consistency of dimensions
        val dimMap = ppMap.mapValues(_.dim)
        val dimL = boundRel.lhs.dim(dimMap)
        val dimR = boundRel.rhs.dim(dimMap)

        // It is allowed to compare 0 with any dimensional values
        if (dimL != dimR && boundRel.lhs != Value(PQZero) && boundRel.rhs != Value(PQZero)) {
          throw new InsufficientConstraintConfigException(this,
            s"Dimension of LHS $dimL is incompatible with dimension of RHS $dimR.")
        }

        // convert related /ProtoParameter/s to /Parameter/s

        // If more than one variables are connected to the same parameter,
        // rename the variables to the same name, since it is not allowed
        // for variables in a /Constraint/ to be connected to the same /Parameter/.
        val pToV: Map[Parameter, Seq[String]] = {
          embodiedParamMap.groupBy(_._2).mapValues(_.keys.toSeq)
        }
        val pf: PartialFunction[(Parameter, Seq[String]), ((String, Parameter), (String, Seq[String]))] = {
          case (p, vs) => {
            val v = vs.head
            (v -> p, (v, vs.tail))
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
          case Rel(RelType.Eq,   l, r) => (ConstraintType.Eq,   l, r)
          case Rel(RelType.Gt,   l, r) => (ConstraintType.InEq, l, r)
          case Rel(RelType.GtEq, l, r) => (ConstraintType.InEq, l, r)
          case Rel(RelType.Lt,   l, r) => (ConstraintType.InEq, r, l)
          case Rel(RelType.LtEq, l, r) => (ConstraintType.InEq, r, l)
        }

        Constraint(id, t, lhs, rhs, newVarParams)
      }
    }
  }
}

object ProtoConstraint {

  def fromXML(source: xml.Node): ProtoConstraint = {
    val id = (source \ "id").text.toInt
    val relStr = (source \ "rel").text
    val paramMap = ((source \\ "var") collect {
      case n if !(n \\ "pid").isEmpty => {
        val nn = n \\ "name"
        val pn = n \\ "pid"
        nn.text -> pn.text.toInt
      }
    }).toMap

    ProtoConstraint(id, relStr, paramMap)
  }
}
