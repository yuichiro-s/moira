package moira.world

import moira.constraint.Parameter
import moira.unit.{SIDim,SIUnit,PhysicalQuantity}
import moira.unit.CommonUnits

// Parameter whose definition can be incomplete.
case class ProtoParameter(
  id: Int = -1,
  name: String = "",
  dim: SIDim = SIDim(),
  displayUnit: String = "",
  lower: Option[PhysicalQuantity] = None,
  upper: Option[PhysicalQuantity] = None,
  value: Option[PhysicalQuantity] = None
) {
  val unit: Option[SIUnit] = CommonUnits.nameToUnit.get(displayUnit)

  // dimensions must be the same
  require(unit match { case Some(u) => u.dim == dim; case None => true })
  require(lower match { case Some(pq) => pq.dim == dim; case None => true })
  require(upper match { case Some(pq) => pq.dim == dim; case None => true })
  require(value match { case Some(pq) => pq.dim == dim; case None => true })

  // make parameter
  lazy val toParameter: Option[Parameter] = {
    (lower, upper) match {
      case (Some(pqL), Some(pqU)) => Some(Parameter(id, dim, pqL, pqU))
      case _ => None
    }
  }
}
