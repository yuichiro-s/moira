package moira.math

case class Matrix(vs: Seq[Seq[Double]]) {
  type M = Seq[Seq[Double]]
  type V = Seq[Double]

  def +(m2: Matrix): Matrix = {
    Matrix(vs.zip(m2.vs) map { v => v._1.zip(v._2).map { d => d._1 + d._2 } })
  }

  def -(m2: Matrix): Matrix = {
    Matrix(vs.zip(m2.vs) map { v => v._1.zip(v._2).map { d => d._1 - d._2 } })
  }

  // matrix-matrix multiplication
  def *(m2: Matrix): Matrix = {
    val vs2 = m2.t.vs
    Matrix(vs map { v1 => vs2 map { v2 => dot(v1, v2) } })
  }

  // matrix-vector multiplication
  def *(v2: V): V = {
    vs map { v1 => dot(v1, v2) }
  }

  lazy val row = vs.size      // # of rows
  lazy val col = vs(0).size   // # of columns

  // transpose
  lazy val t = Matrix(vs.transpose)

  // Solves linear equation Ax=b.
  def linSolve(b0: V): Option[V] = {
    require(row == col) // b has to be a square matrix.
    require(b0.size == row) // b has to be a square matrix.

    type State = (M, V)

    def pivot(s: State): Option[State] = {
      val m = s._1
      val b = s._2

      val (a, i) = m.map(_(0)).zipWithIndex.maxBy { x: (Double, Int) => Math.abs(x._1) }

      if (a == 0) {
        // matrix is not regular
        None
      } else {
        val m1 = m.take(i)
        val m2 = m.drop(i+1)

        val b1 = b.take(i)
        val b2 = b.drop(i+1)

        val pivRow = m(i).map(_ / a)
        val newB = b(i) / a

        Some((pivRow+:m1)++m2, (newB+:b1)++b2)
      }
    }

    def forwardElimination(s: State): Option[State] = {
      if (s._2.size == 0) {
        // terminate recursion
        Some((Seq(), Seq()))
      } else {
        pivot(s) match {
          case None => None
          case Some((mp, bp)) => {
            val pivM = mp(0)
            val pivB = bp(0)

            val nextState = (mp.zip(bp).tail map { mb =>
              val row = mb._1
              val v = mb._2
              val a0 = row(0)
              (row.zip(pivM).tail map { t => t._1 - a0 * t._2 }, v - a0 * pivB)
            }).unzip

            forwardElimination(nextState) match {
              case None => None
              case Some((smallerM, smallerB)) => {
                // construct bigger M and B
                Some((pivM +: smallerM.map(0.0 +: _), pivB +: smallerB))
              }
            }
          }
        }
      }

    }

    def backSubstitution(s: State): V = {
      val m = s._1
      val b = s._2
      val n = b.size
      if (n == 0) {
        Seq()
      } else {
        val v = b.last
        val newB = b.zip(m.map(_(n-1))).init.map { t => t._1 - v * t._2 }
        backSubstitution((m, newB)) :+ v
      }
    }

    forwardElimination((vs, b0)) match {
      case Some(s) => {
        Some(backSubstitution(s))
      }
      case None => None
    }
  }

  def dot(v1: Seq[Double], v2: Seq[Double]): Double = {
    (v1.zip(v2) map { d => d._1 * d._2 }).sum
  }

}

