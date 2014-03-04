package moira.world

import moira.unit.SIDim
import moira.unit.SIUnit
import moira.unit.PhysicalQuantity

// possibly incomplete constraints and parameters
// This is what the user manipulates.
case class World(
  val constraints: Set[ProtoConstraint],
  val parameters: Set[ProtoParameter],
  val nextConstraintId: Int = 0,
  val nextParameterId: Int = 0
) {

  require(constraints.map(_.id).toSet.size == constraints.size,
    "No two constraints should have the same id.")

  require(parameters.map(_.id).toSet.size == parameters.size,
    "No two parameters should have the same id.")

  // actions to /Constraint/

  def newConstraint(
    relStr: String,
    paramMap: Map[String, ProtoParameter]
  ): (World, ProtoConstraint) = {
    val newConst = ProtoConstraint(
      nextConstraintId,
      relStr,
      paramMap
    )
    val newWorld = copy(
      constraints = constraints + newConst,
      nextConstraintId = nextConstraintId + 1
    )
    (newWorld, newConst)
  }

  def copyConstraint(const: ProtoConstraint): (World, ProtoConstraint) = {
    require(constraints.contains(const))

    val newConst = const.copy(id = nextConstraintId)
    val newWorld = copy(
      constraints = constraints + newConst,
      nextConstraintId = nextConstraintId + 1
    )
    (newWorld, newConst)
  }

  def updateConstraint(
    const: ProtoConstraint,
    relStr: String,
    paramMap: Map[String, ProtoParameter]
  ): (World, ProtoConstraint) = {
    require(constraints.contains(const))

    val newConst = const.copy(
      relStr = relStr,
      paramMap = paramMap
    )
    val newWorld = removeConstraint(const)
    val newWorld2 = newWorld.copy(constraints = newWorld.constraints + newConst)
    (newWorld2, newConst)
  }

  def removeConstraint(const: ProtoConstraint): World = {
    require(constraints.contains(const))

    copy(constraints = constraints - const)
  }

  def addConnection(const: ProtoConstraint, v: String, param: ProtoParameter): (World, ProtoConstraint) = {
    require(constraints.contains(const))
    require(parameters.contains(param))
    require(const.vars match { case None => false; case Some(vs) => vs.contains(v) })

    updateConstraint(const, const.relStr, const.paramMap + (v -> param))
  }

  def removeConnection(const: ProtoConstraint, v: String): (World, ProtoConstraint) = {
    require(constraints.contains(const))

    updateConstraint(const, const.relStr, const.paramMap - v)
  }

  // actions to /Parameter/

  def newParameter(
    name: String,
    dim: SIDim,
    displayUnit: SIUnit,
    lower: Option[PhysicalQuantity],
    upper: Option[PhysicalQuantity],
    value: Option[PhysicalQuantity]
  ): (World, ProtoParameter) = {
    val newParam = ProtoParameter(
      nextParameterId,
      name,
      dim,
      displayUnit,
      lower,
      upper,
      value
    )
    val newWorld = copy(
      parameters = parameters + newParam,
      nextParameterId = nextParameterId + 1
    )
    (newWorld, newParam)
  }

  def copyParameter(param: ProtoParameter): (World, ProtoParameter) = {
    require(parameters.contains(param))
    val newParam = param.copy(id = nextParameterId)
    val newWorld = copy(
      parameters = parameters + newParam,
      nextParameterId = nextParameterId + 1
    )
    (newWorld, newParam)
  }

  def updateParameter(
    param: ProtoParameter,
    name: String,
    dim: SIDim,
    displayUnit: SIUnit,
    lower: Option[PhysicalQuantity],
    upper: Option[PhysicalQuantity],
    value: Option[PhysicalQuantity]
  ): (World, ProtoParameter) = {
    require(parameters.contains(param))
    val newParam = param.copy(
      name = name,
      dim = dim,
      displayUnit = displayUnit,
      lower = lower,
      upper = upper,
      value = value
    )
    val newWorld = removeParameter(param)
    val newWorld2 = newWorld.copy(parameters = newWorld.parameters + newParam)
    (newWorld2, newParam)
  }

  def removeParameter(param: ProtoParameter): World = {
    copy(parameters = parameters - param)
  }

  /*lazy val toXML: xml.Elem = {

  }*/
}
