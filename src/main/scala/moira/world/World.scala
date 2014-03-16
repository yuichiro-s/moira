package moira.world

import moira.unit.SIDim
import moira.unit.PhysicalQuantity

// possibly incomplete constraints and parameters
// This is what the user manipulates.
case class World(
  constraints: Set[ProtoConstraint],
  parameters: Set[ProtoParameter],
  nextConstraintId: Int = 0,
  nextParameterId: Int = 0
) {

  require(constraints.map(_.id).size == constraints.size,
    "No two constraints should have the same id.")

  require(parameters.map(_.id).size == parameters.size,
    "No two parameters should have the same id.")

  // actions to /Constraint/

  // Common function to process a constraint with the given /id/.
  def withConstraintId[T](id: Int)(f: => ProtoConstraint => T): T = {
    constraints.find(_.id == id) match {
      case Some(c) => f(c)
      case None => {
        // fails if a /ProtoConstraint/ with the /id/ is not found.
        throw new IllegalArgumentException(
          "ProtoConstraint(id=%d) not found in %s.".format(id, this))
      }
    }
  }

  def createConstraint(
    relStr: String,
    paramMap: Map[String, Int]
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

  def copyConstraint(id: Int): (World, ProtoConstraint) = withConstraintId(id) { c =>
    val newConst = c.copy(id = nextConstraintId)
    val newWorld = copy(
      constraints = constraints + newConst,
      nextConstraintId = nextConstraintId + 1
    )
    (newWorld, newConst)
  }

  def updateConstraint(
    id: Int,
    relStr: String,
    paramMap: Map[String, Int]
  ): (World, ProtoConstraint) = withConstraintId(id) { c =>
    val newConst = c.copy(
      relStr = relStr,
      paramMap = paramMap
    )
    val newWorld = removeConstraint(id)
    val newWorld2 = newWorld.copy(constraints = newWorld.constraints + newConst)
    (newWorld2, newConst)
  }

  def removeConstraint(id: Int): World = withConstraintId(id) { c =>
    copy(constraints = constraints - c)
  }

  def addConnection(cId: Int, v: String, pId: Int): (World, ProtoConstraint) = {
    withConstraintId(cId) { c =>
      withParameterId(pId) { p =>
        require(c.vars match { case None => false; case Some(vs) => vs.contains(v) },
          "ProtoConstraint(id=%d) must contain a variable %s.".format(cId, v))
        updateConstraint(cId, c.relStr, c.paramMap + (v -> pId))
      }
    }
  }

  def removeConnection(cId: Int, v: String): (World, ProtoConstraint) = {
    withConstraintId(cId) { c =>
      updateConstraint(cId, c.relStr, c.paramMap - v)
    }
  }

  // actions to /Parameter/

  // Common function to process a parameter with the given /id/.
  def withParameterId[T](id: Int)(f: => ProtoParameter => T): T = {
    parameters.find(_.id == id) match {
      case Some(p) => f(p)
      case None => {
        // fails if a /ProtoParameter/ with the /id/ is not found.
        throw new IllegalArgumentException(
          "ProtoParameter(id=%d) not found in %s.".format(id, this))
      }
    }
  }

  def createParameter(
    name: String,
    dim: SIDim,
    displayUnit: String,
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

  def copyParameter(id: Int): (World, ProtoParameter) = {
    withParameterId(id) { p =>
      val newParam = p.copy(id = nextParameterId)
      val newWorld = copy(
        parameters = parameters + newParam,
        nextParameterId = nextParameterId + 1
      )
      (newWorld, newParam)
    }
  }

  def updateParameter(
    id: Int,
    name: String,
    dim: SIDim,
    displayUnit: String,
    lower: Option[PhysicalQuantity],
    upper: Option[PhysicalQuantity],
    value: Option[PhysicalQuantity]
  ): (World, ProtoParameter) = withParameterId(id) { p =>
    val newParam: ProtoParameter = p.copy(
      name = name,
      dim = dim,
      displayUnit = displayUnit,
      lower = lower,
      upper = upper,
      value = value
    )
    var newWorld = removeParameter(id)
    newWorld = newWorld.copy(parameters = newWorld.parameters + newParam)

    (newWorld, newParam)
  }

  def removeParameter(id: Int): World = {
    withParameterId(id) { p =>
      copy(parameters = parameters - p)
    }
  }

  // getter

  def getParameterById(id: Int): Option[ProtoParameter] = {
    parameters.find(_.id == id)
  }

  def getConstraintById(id: Int): Option[ProtoConstraint] = {
    constraints.find(_.id == id)
  }

}
