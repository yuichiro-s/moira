package moira.constraint.solver

import moira.constraint.Parameter
import moira.constraint.Constraint
import moira.unit.SIDim
import moira.unit.SIUnit
import moira.unit.PhysicalQuantity
import moira.math.NumericalSolver

object ConstraintSolver {
  type Bindings = Map[Parameter, PhysicalQuantity]
  type BindingCandidates = Seq[Bindings]

  type Strategy = Set[Constraint] => BindingCandidates

  val eps = 1e-5
  val strategies: Seq[Strategy] = List(
    solveUnaryEquality,
    solveBinaryEqualities,
    solveNaryEqualities(3),
    bindMostConstrainedVariable(5)
  ) // TODO: change strategies based on the number of constraints

  // generate one possible value of the parameter according to its lower and upper
  def genValue(param: Parameter): PhysicalQuantity = {
    val u = param.upper.normalized
    val l = param.lower.normalized
    val v = (u.value - l.value) * scala.util.Random.nextDouble() + l.value
    PhysicalQuantity(v, u.unit)
  }

  // Checks if all values are inside the range of allowed values of the parameters
  def checkRange(bs: Bindings, eps: Double=1e-5): Boolean = {
    bs forall {
      case (param, pq) => {
        val l: Double = param.lower.normalized.value
        val p: Double = pq.normalized.value
        val u: Double = param.upper.normalized.value
        NumericalSolver.greater(u, p, eps) && NumericalSolver.greater(p, l, eps)
      }
    }
  }

  def solve(cs: Set[Constraint]): Option[Bindings] = {
    if (cs.size == 0) {
      // successfully solved
      Some(Map.empty)
    } else {
      // use iterator so that all strategies don't get used at once
      strategies.iterator.map(_(cs)).foreach { bss =>
        bss.foreach { bs0 =>
          if (checkRange(bs0)) {
            val newCs = cs.map(_.bind(bs0))
            val (constants, restCs) = newCs.partition(_.isConstant)

            // check if all constant constraints are satisfied under this bindings
            val isValid = constants.forall(_.satisfied(eps) == Some(true))
            if (isValid) {
              solve(restCs) match {  // solve remaining constraints
                case None => {
                  // impossible to solve with the current bindings
                  // try with next candidate
                }
                case Some(bs) => {
                  // remaining constraints are solved
                  return Some(bs0 ++ bs)  
                }
              }
            }
          }
        }
        // it is not possible to solve with any candidate solutions
        // returned by the current strategy
      }
      // all strategies have been tried but no solution was found
      None
    }
  }

  def solveUnaryEquality(cs: Set[Constraint]): BindingCandidates = {
    val cs2: Set[Constraint] = cs filter {
      c: Constraint => c.isUnary && c.isEquality
    }
    if (cs2.size == 0) return Seq.empty  // no unary equality found

    val candidates: Map[Parameter, Set[(Parameter, Double, SIDim)]] = cs2 map { c =>
      val e = c.toExpr
      val v = e.vars.toSeq.head      // name of variable
      val p = c.paramMap(v)             // assumes that corresponding parameter exists
      val f = e.toUnaryFunc(v, p.dim)         // function to search the solution
      val x0 = genValue(p).normalized.value   // initial value for newton's solver
      NumericalSolver.newtonsSolver(f, x0) match {
        case None => return Seq()   // there is an unsatisfiable equation, so fails
        case Some(d) => (p, d, p.dim) // (parameter, value, dimension)
      }
    } groupBy(_._1)
    val bs: Map[Parameter, PhysicalQuantity] = candidates map { case (p, vals) =>
      val sol: (Parameter, Double, SIDim) = vals reduce { (val1, val2) => {
        val d1 = val1._2
        val d2 = val2._2
        val dim1 = val1._3
        val dim2 = val2._3
        if (NumericalSolver.equal(d1, d2) && dim1 == dim2) val1
        else return Seq.empty  // there are equations with different solutions
                          // for the same variable
        }
      }
      p -> PhysicalQuantity(sol._2, SIUnit(1.0, sol._3))
    }
    assert(bs.size > 0)
    Seq(bs)
  }

  def findBinaryEqualities(cs: Set[Constraint]): Option[(Constraint, Constraint, Parameter, Parameter)] = {
    // takes linear time to find the equations
    val m = collection.mutable.Map[(Parameter, Parameter), Constraint]()

    cs.iterator.filter { c: Constraint => c.isEquality && c.arity == 2 } foreach { c =>
      c.params.toList match {
        case p1::p2::Nil => {
          // check if this pair already exists
          m.get((p1, p2)) match {
            case Some(c2) => return Some((c, c2, p1, p2))
            case None => m.get((p2, p1)) match {
              case Some(c2) => return Some((c, c2, p1, p2))
              case None => Unit  // this return value is ignored
            }
          }
          m += ((p1, p2) -> c)
          m += ((p2, p1) -> c)
        }
        case _ => { assert(false, "Should be unreacheable.") }
      }
    }
    None  // no 2 equations have the same 2 variables
  }

  // Finds at most n equalities which have n parameters in common.
  /*def findUptoNaryEqualities(n: Int, cs: Set[Constraint]): Option[(Set[Constraint], Set[Parameter])] = {
    // First of all, parameters which have only one related constraint
    // don't have to be taken into consideration.
    // PROOF: If n constraints have exactly n parameters in common and
    // there is a parameter which has only one related constraint,
    // removing the constraint and the parameter leaves n-1 constraints
    // which have exactly n-1 parameters in common.
  }*/
  
  // Finds n equalities which have exactly n parameters in common.
  def findNaryEqualities(n: Int, cs: Set[Constraint]): Option[(Set[Constraint], Set[Parameter])] = {

    cs.toSeq.combinations(n) foreach { comb: Seq[Constraint] =>
      // common parameters
      val ps: Set[Parameter] = comb.map(_.params).reduce(_++_)

      if (ps.size == n) {
        // these n constraints have exactly n parameters in common
        return Some((comb.toSet, ps))
      }
    }
    None  // no set of constraints found
  }

  // Solves the given constraints 
  def solveConstraintsWithParameters(cs: Seq[Constraint], ps: Seq[Parameter]): BindingCandidates = {
    // union of parameter sets of cs have to equal ps
    require(cs.map(_.params).reduce(_++_) == ps.toSet)

    // function to be solved using Newton's method
    val f: Seq[Seq[Double] => Double] = cs map { c =>
      val vs = ps map { p =>
        c.paramMap.find(_._2 == p) match {
          case Some((name, _)) => Some(name -> p.dim)
          case None => None   // parameter p doesn't appear in c
        }
      }
      c.toExpr.toFunc(vs)
    }

    // initial vector for Newton's method
    val x0 = ps.map(genValue(_).normalized.value)

    // solution
    val sol: Seq[Double] = NumericalSolver.multiNewtonsSolver(f, x0) match {
      case None => return Seq.empty   // No solution to this system of equations
      case Some(ds) => ds
    }

    // reconstruct physical quantities from the solution
    val bs = sol.zip(ps) map {
      case (d, p) => p -> PhysicalQuantity(d, SIUnit(1.0, p.dim))
    }
    Seq(bs.toMap)
  }

  // Finds 2 equations of 2 variables, and solves them.
  def solveBinaryEqualities(cs: Set[Constraint]): BindingCandidates = {
    findBinaryEqualities(cs) match {
      case Some((c1, c2, p1, p2)) => {
        val cs = Seq(c1, c2)
        val ps = Seq(p1, p2)
        solveConstraintsWithParameters(cs, ps)
      }
      case _ => Seq.empty  // no pair of binary equalities found
    }
  }

  // Finds N equations of N variables, and solves them.
  def solveNaryEqualities(n: Int)(cs: Set[Constraint]): BindingCandidates = {
    findNaryEqualities(n, cs) match {
      case Some((cs, ps)) => {
        solveConstraintsWithParameters(cs.toSeq, ps.toSeq)
      }
      case _ => Seq.empty
    }
  }

  // Picks the most constrained variable and binds it randomly.
  def bindMostConstrainedVariable(trialNum: Int)(cs: Set[Constraint]): BindingCandidates = {
    val constraintCount = collection.mutable.Map[Parameter, Int]()
    // count the occurrence of each parameter
    cs foreach { c => 
      c.params foreach { p =>
        constraintCount += p -> (constraintCount.getOrElse(p, 0) + 1)
      }
    }
    // sequence of most constrained variables
    val mostConstrainedPs: Seq[Parameter] = {
      val maxCount: Int = constraintCount.maxBy(_._2)._2
      constraintCount.filter(_._2 == maxCount).keys.toSeq
    }

    1 to trialNum map { _ =>
      // randomly pick one of the most constrained variables
      val idx = util.Random.nextInt(mostConstrainedPs.size)
      val p = mostConstrainedPs(idx)
      val pq = genValue(p)

      Map(p -> pq)
    }
  }
}

