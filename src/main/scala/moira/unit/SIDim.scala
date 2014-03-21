package moira.unit

import scala.util.parsing.combinator.JavaTokenParsers

// Dimension
case class SIDim(m: Int = 0, kg: Int = 0, s: Int = 0, a: Int = 0, k: Int = 0, cd: Int = 0, mol: Int = 0) {
  def *(dim: SIDim): SIDim =
    SIDim(m+dim.m, kg+dim.kg, s+dim.s, a+dim.a, k+dim.k, cd+dim.cd, mol+dim.mol)

  def /(dim: SIDim): SIDim =
    SIDim(m-dim.m, kg-dim.kg, s-dim.s, a-dim.a, k-dim.k, cd-dim.cd, mol-dim.mol)

  def **(n: Int): SIDim =
    SIDim(m*n, kg*n, s*n, a*n, k*n, cd*n, mol*n)

  override def toString = {
    var str = ""
    Seq((kg, "M"),
      (m, "L"),
      (s, "T"),
      (a, "I"),
      (k, "Th"),
      (mol, "N"),
      (cd, "J")
    ) foreach { case (n, s) =>
      if (n != 0) {
        str += s
        if (n != 1) str += n
      }
    }
    if (str == "") SIDim.NO_DIM_STR else str
  }
}

object SIDim {
  val NO_DIM_STR = "NODIM"

  // create /SIDim/ from /str/
  def fromStr(str: String): Option[SIDim] = {
    val parser = new JavaTokenParsers {
      def p(str: String, dim: SIDim): Parser[SIDim] = str~>opt(wholeNumber) ^^ {
        case None => dim
        case Some(n) => dim ** n.toInt
      }

      val kg = p("M", SIDim(kg = 1))
      val m = p("L", SIDim(m = 1))
      val s = p("T", SIDim(s = 1))
      val a = p("I", SIDim(a = 1))
      val k = p("Th", SIDim(k = 1))
      val mol = p("N", SIDim(mol = 1))
      val cd = p("J", SIDim(cd = 1))

      val simple: Parser[SIDim] = kg | m | s | a | k | mol | cd
      val complex: Parser[SIDim] = rep(simple) ^^ {
        case ss => ss.reduce(_ * _)
      }

      def parse(str: String): Option[SIDim] = {
        parseAll(complex, str) match {
          case Success(dim, _) => Some(dim)
          case _ => None
        }
      }
    }

    if (str == NO_DIM_STR) {
      Some(SIDim())
    } else {
      parser.parse(str)
    }
  }

}

