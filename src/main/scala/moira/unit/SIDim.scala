package moira.unit

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
    if (str == "") "<NODIM>" else str
  }
}

