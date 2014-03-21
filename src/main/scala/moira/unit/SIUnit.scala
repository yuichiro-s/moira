package moira.unit

// Unit
case class SIUnit(factor: Double = 1.0, dim: SIDim = SIDim()) {
  def *(unit: SIUnit): SIUnit =
    SIUnit(factor*unit.factor, dim*unit.dim)

  def /(unit: SIUnit): SIUnit =
    SIUnit(factor/unit.factor, dim/unit.dim)

  def **(n: Int): SIUnit =
    SIUnit(Math.pow(factor, n), dim**n)

}
