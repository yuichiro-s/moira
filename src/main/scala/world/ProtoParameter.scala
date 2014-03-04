package moira.world

import moira.constraint.Parameter
import moira.unit.SIDim
import moira.unit.SIUnit
import moira.unit.PhysicalQuantity

// Parameter whose definition can be incomplete.
case class ProtoParameter(
  id: Int,
  name: String,
  dim: SIDim,
  displayUnit: SIUnit,
  lower: Option[PhysicalQuantity],
  upper: Option[PhysicalQuantity],
  value: Option[PhysicalQuantity]
) {
  // dimensions must be the same
  require(displayUnit.dim == dim)
  require(lower match { case Some(pq) => pq.dim == dim; case None => true })
  require(upper match { case Some(pq) => pq.dim == dim; case None => true })
  require(value match { case Some(pq) => pq.dim == dim; case None => true })

  // update
  def updateName(newName: String) = copy(name = newName)
  def updateDim(newDim: SIDim) = {
    copy(dim = newDim, displayUnit = SIUnit(1.0, newDim), lower=None, upper=None, value=None)
  }
  def updateDisplayUnit(newDU: SIUnit) = {
    copy(displayUnit = newDU)
  }
  def updateLower(newLower: Option[PhysicalQuantity]) = copy(lower=newLower)
  def updateUpper(newUpper: Option[PhysicalQuantity]) = copy(upper=newUpper)
  def updateValue(newValue: Option[PhysicalQuantity]) = copy(value=newValue)

  // make parameter
  lazy val toParameter: Option[Parameter] = {
    (lower, upper) match {
      case (Some(pqL), Some(pqU)) => Some(Parameter(id, dim, pqL, pqU))
      case _ => None
    }
  }
}
