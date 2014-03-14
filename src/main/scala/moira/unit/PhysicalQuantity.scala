package moira.unit

// Physical Quantity
case class PhysicalQuantity(value: Double, unit: SIUnit = SIUnit()) {
  lazy val dim = unit.dim
  val isZero = value == 0

  def +(pq: PhysicalQuantity): PhysicalQuantity = {
    require(isZero || pq.isZero || dim == pq.dim,
      s"Addition of incompatible quantities $this and $pq.")
    val pq1 = normalized
    val pq2 = pq.normalized
    PhysicalQuantity(pq1.value + pq2.value, pq1.unit)
  }

  def -(pq: PhysicalQuantity): PhysicalQuantity = {
    require(isZero || pq.isZero || dim == pq.dim,
      s"Subtraction of incompatible quantities $this and $pq.")
    val pq1 = normalized
    val pq2 = pq.normalized
    PhysicalQuantity(pq1.value - pq2.value, pq1.unit)
  }

  def *(pq: PhysicalQuantity): PhysicalQuantity =
    PhysicalQuantity(value * pq.value, unit * pq.unit)

  def /(pq: PhysicalQuantity): PhysicalQuantity =
    PhysicalQuantity(value / pq.value, unit / pq.unit)

  def *(d: Double): PhysicalQuantity =
    PhysicalQuantity(value * d, unit)

  def /(d: Double): PhysicalQuantity =
    PhysicalQuantity(value / d, unit)

  def **(n: Int): PhysicalQuantity =
    PhysicalQuantity(Math.pow(value, n), unit ** n)

  // Converts to an equivalent /PhysicalQuantity/ with the specified unit.
  def convertUnit(newUnit: SIUnit): PhysicalQuantity = {
    require(newUnit.dim == dim)
    PhysicalQuantity(value * unit.factor / newUnit.factor, newUnit)
  }

  // get an equivalent physical quantity with a unit whose factor is 1
  lazy val normalized: PhysicalQuantity = {
    val newValue = value * unit.factor
    PhysicalQuantity(newValue, SIUnit(1.0, dim))
  }

  // pretty-printing
  override def toString = {
    CommonUnits.unitToName.get(unit) match {
      case None => super.toString
      case Some(str) => {
        if (dim == CommonDims.NODIM) {
          "%.2f".format(value * unit.factor)
        } else {
          "%.2f %s".format(value, str)
        }
      }
    }
  }
}

object PQZero extends PhysicalQuantity(0.0)
object PQOne extends PhysicalQuantity(1.0)
