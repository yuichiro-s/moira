package moira.unit

// Physical Quantity
case class PhysicalQuantity(value: Double, unit: SIUnit = SIUnit()) {
  lazy val dim = unit.dim
  val isZero = value == 0

  def +(pq: PhysicalQuantity): PhysicalQuantity = {
    require(isZero || pq.isZero || dim == pq.dim)
    val pq1 = normalized
    val pq2 = pq.normalized
    PhysicalQuantity(pq1.value + pq2.value, pq1.unit)
  }

  def -(pq: PhysicalQuantity): PhysicalQuantity = {
    require(isZero || pq.isZero || dim == pq.dim)
    val pq1 = normalized
    val pq2 = pq.normalized
    PhysicalQuantity(pq1.value - pq2.value, pq1.unit)
  }

  def *(pq: PhysicalQuantity): PhysicalQuantity =
    PhysicalQuantity(value * pq.value, unit * pq.unit)

  def /(pq: PhysicalQuantity): PhysicalQuantity =
    PhysicalQuantity(value / pq.value, unit / pq.unit)

  def **(n: Int): PhysicalQuantity =
    PhysicalQuantity(Math.pow(value, n), unit ** n)

  // get an equivalent physical quantity with a unit whose factor is 1
  lazy val normalized: PhysicalQuantity = {
    val newValue = value * unit.factor
    PhysicalQuantity(newValue, SIUnit(1.0, dim))
  }

  // pretty-printing
  override def toString = {
    CommonUnits.unitToName.get(unit) match {
      case None => super.toString
      case Some(str) => "%.1f %s".format(value, str)
    }
  }
}

object PQZero extends PhysicalQuantity(0.0)
object PQOne extends PhysicalQuantity(1.0)
