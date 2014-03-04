import org.scalatest.FlatSpec

import moira.expression._
import moira.constraint._
import moira.constraint.solver._

class ConstraintSpec extends FlatSpec {

  val parser = Parser
  def e(str: String) = parser.parseExpr(str).get
  def r(str: String) = parser.parseRel(str).get
  def pq(str: String) = e(str) match { case Value(pq) => pq }

  def c(id: Int, relation: String, params: (String, Parameter) *) = {
    val rel = r(relation)
    Constraint(id, ConstraintType.Eq, rel.lhs, rel.rhs, params.toMap)
  }

  def ci(id: Int, relation: String, params: (String, Parameter) *) = {
    val rel = r(relation)
    Constraint(id, ConstraintType.InEq, rel.lhs, rel.rhs, params.toMap)
  }

  def p(id: Int, lower: String, upper: String) = {
    val lowerPQ = pq(lower)
    val upperPQ = pq(upper)
    Parameter(id, lowerPQ.unit.dim, lowerPQ, upperPQ)
  }

  "A constraint of 1 variable" should "be solved" in {
    val p1 = p(1, "0 m", "10 km")

    val c1 = c(1, "$x + 1 m = 5 km", "x" -> p1)
    // TODO: define BinOp and change definition of Rel(so that you can call lhs&rhs)
    // TODO: reason about the relationship between Rel and Constraint

    val ans = ConstraintSolver.solve(Set(c1))
  }

  "2 constraints of 2 variables" should "be solved" in {
    val p1 = p(1, "0 m", "10 km")
    val p2 = p(2, "0 m", "10 km")
    val p3 = p(3, "0 m", "10 km")

    val c1 = c(1, "$x + $y = 5 km","x"->p1, "y"->p2)
    val c2 = c(2, "$x + $y + $z = 7 km", "x"->p1, "y"->p2, "z"->p3)
    val c3 = c(3, "$x + 1 km - $y = 5 km", "x"->p1, "y"->p2)

    val ans = ConstraintSolver.solve(Set(c1, c2, c3))
  }

  // TODO: 2 consistent constraints of 1 variable
  // TODO: 2 inconsistent constraints of 1 variable

  // TODO: 3 consistent constraints of 1 variable
  // TODO: 2 consistent constraints and 1 inconsistent constraint of 1 variable

  "4 constraints of 4 variables" should "be solved" in {
    val p1 = p(1, "0 m", "3.9 km")
    val p2 = p(2, "0 m", "3.9 km")
    val p3 = p(3, "0 m", "3.9 km")
    val p4 = p(4, "0 m", "3.9 km")

    val c1 = c(1, "$x + 1 km = $y", "x"->p1, "y"->p2)
    val c2 = c(2, "$y * 3 = $z * 2", "y"->p2, "z"->p3)
    val c3 = c(3, "$x + $y = $z", "x"->p1, "y"->p2, "z"->p3)
    val c4 = c(4, "$z + $w = 7 km", "z"->p3, "w"->p4)

    val ans = ConstraintSolver.solve(Set(c1, c2, c3, c4))
  }

  "6 constraints of 6 variables" should "be solved" in {
    val pa = p(1, "-10 km", "10 km")
    val pb = p(2, "-10 km", "10 km")
    val pc = p(3, "-10 km", "10 km")
    val px = p(4, "-10 km", "10 km")
    val py = p(5, "-10 km", "10 km")
    val pz = p(6, "-10 km", "10 km")

    val c1 = ci(1, "$a+$x>2m",
      "a"->pa, "x"->px)
    val c2 = c(2, "$a+$b=2m",
      "a"->pa, "b"->pb)
    val c3 = c(3, "$b+$c=2m",
      "b"->pb, "c"->pc)
    val c4 = c(4, "$c+$y=2m",
      "c"->pc, "y"->py)
    val c5 = c(5, "$x+$y+$z=3m",
      "x"->px, "y"->py, "z"->pz)
    val c6 = c(6, "$x*$y*$z=1m3",
      "x"->px, "y"->py, "z"->pz)

    val ans = ConstraintSolver.solve(Set(c1, c2, c3, c4, c5, c6))
  }
}
