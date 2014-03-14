package moira.math

object NumericalSolver {

  type V = Seq[Double]
  type M = Seq[Seq[Double]]

  // Checks if difference is almost 0
  def equal(a: Double, b: Double, eps: Double=1e-5): Boolean = {
    // When either value is exactly 0, it's impossible to tell
    // whether the other one is close enough to 0, so just compare it with eps.
    if (a == 0) { Math.abs(b) < eps }
    else if (b == 0) { Math.abs(a) < eps }
    else Math.abs(a-b) <= Math.max(Math.abs(a), Math.abs(b)) * eps
  }

  // Checks if difference is larger than 0
  def greater(a: Double, b: Double, eps: Double=1e-5): Boolean = {
    if (a == 0) { eps > b }
    else if (b == 0) { a > -eps }
    a - b >= -Math.max(Math.abs(a), Math.abs(b)) * eps
  }

  def diff(f: Double => Double, x: Double, h0: Double, eps: Double): Option[Double] = {
    def easydiff(h: Double) = (f(x+h) - f(x)) / h
    def elimerror(n: Long, s: Stream[Double]): Stream[Double] = {
      val a = s.head
      val b = s.tail.head
      val res = (b * (1 << n) - a) / ((1 << n) - 1)
      Stream.cons(res, elimerror(n, s.tail))
    }
    def order(s: Stream[Double]) = {
      val a = s.head
      val b = s.tail.head
      val c = s.tail.tail.head
      Math.round(Math.log((a-c)/(b-c)-1) / Math.log(2))
    }
    def improve(s: Stream[Double]) = elimerror(order(s), s) 
    def sup(s: Stream[Double]): Stream[Double] =
      Stream.iterate(s)(improve).map { x => x.tail.head }

    val differentiate = Stream.iterate(h0)(x => x/2).map(easydiff)

    relative(sup(differentiate).map(Some(_)), eps) match {
      case Some(d) => Some(d)
      case None => relative(differentiate.map(Some(_)), eps)  // try without using sup
    }
  }

  def relative(s: Stream[Option[Double]], eps: Double, n: Int=100): Option[Double] = {
    def test(a: Double, b: Double) = {
      if (Math.abs(b).isInfinite) {
        (true, None)
      } else if (Math.abs(a - b) <= eps * Math.abs(b)) {
        (true, Some(b))
      } else {
        (false, None)
      }
    }
    converge(s, test, eps, n)
  }

  def converge[T](s: Stream[Option[T]], f: (T, T) => (Boolean, Option[T]), eps: Double, n: Int=100): Option[T] = {
    if (n <= 0) {
      // exceeded number of iteration
      None
    } else {
      val a = s.head
      val b = s.tail.head
      (a, b) match {
        case (Some(a_), Some(b_)) => {
          f(a_, b_) match {
            case (true, res) => res
            case (false, _) => converge(s.tail, f, eps, n-1)
          }
        }
        case _ => None
      }
    }
  }

  // Calculates jacobian of f[nxm] at x0[m].
  // Returns None if any one of the elements is None.
  def jacobian(f: Seq[V => Double], x0: V, eps: Double): Option[M] = {
    Some(f map { func =>
      x0.zipWithIndex map { case (xi, i) =>
        val fi = { x: Double => func(x0.updated(i, x))}
        diff(fi, xi, eps, eps) match {
          case Some(d) => d
          case None => return None
        }
      }
    })
  }

  // Solves the equation f(x) = 0, with the initial guess x = x0.
  // Returns None if answer is not found.
  def newtonsSolver(f: Double => Double, x0: Double, eps: Double=1e-5, n: Int=100): Option[Double] = {
    def updater(x: Option[Double]): Option[Double] = {
      x match {
        case Some(x_) => {
          diff(f, x_, eps, eps) match {
            case Some(d_) => Some(x_ - f(x_) / d_)
            case None => None
          }
        }
        case None => None
      }
    }
    relative(Stream.iterate[Option[Double]](Some(x0))(updater), eps, n)
  }

  // Solves a system of n equations of n variables.
  def multiNewtonsSolver(f: Seq[V => Double], x0: V, eps: Double=1e-5, n: Int=100): Option[V] = {

    def updater(x: Option[V]): Option[V] = {
      x match {
        case None => None
        case Some(x_) => {
          jacobian(f, x_, eps) match {
            case None => None
            case Some(j) => {
              val fx = f.map(_(x_))
              Matrix(j).linSolve(fx) match {
                case None => None
                case Some(r) => Some(x_.zip(r) map { case (xi, ri) => xi - ri })
              }
            }
          }
        }
      }
    }

    def conv(s: Stream[Option[V]], eps: Double, n: Int=100): Option[V] = {
      def test(a: V, b: V) = {
        if (b.exists(Math.abs(_).isInfinite)) {
          (true, None)
        } else if (a.zip(b) forall { case (ai, bi) => Math.abs(ai - bi) <= eps * Math.abs(bi) }) {
          (true, Some(b))
        } else {
          (false, None)
        }
      }
      converge(s, test, eps, n)
    }

    conv(Stream.iterate[Option[V]](Some(x0))(updater), eps, n)
  }

}
