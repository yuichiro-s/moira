package moira.world

import moira.constraint.Parameter
import moira.unit.{SIDim,SIUnit,PhysicalQuantity}
import moira.unit.CommonUnits
import scala.xml.Node

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

  // whether it's ready to be converted to a /Parameter/
  lazy val isWellDefined: Boolean = {
    (lower, upper) match {
      case (Some(pqL), Some(pqU)) => {
        // check whether upper >= lower
        (pqU - pqL).value >= 0
      }
      case _ => false   // Either lower or upper is not defined.
    }
  }

  // make parameter
  lazy val toParameter: Option[Parameter] = {
    (lower, upper) match {
      case (Some(pqL), Some(pqU)) if isWellDefined => Some(Parameter(id, dim, pqL, pqU))
      case _ => None
    }
  }
}


object ProtoParameter {
  def fromXML(source: xml.Node): ProtoParameter = {
    val id: Int = (source \ "id").text.toInt
    val name: String = (source \ "name").text
    val dim: SIDim = {
      val n: Node = (source \ "dim").head
      val L =  (n \ "L" ) match { case Seq() => 0; case s => s.text.toInt }
      val M =  (n \ "M" ) match { case Seq() => 0; case s => s.text.toInt }
      val T =  (n \ "T" ) match { case Seq() => 0; case s => s.text.toInt }
      val I =  (n \ "I" ) match { case Seq() => 0; case s => s.text.toInt }
      val Th = (n \ "Th") match { case Seq() => 0; case s => s.text.toInt }
      val N =  (n \ "N" ) match { case Seq() => 0; case s => s.text.toInt }
      val J =  (n \ "J" ) match { case Seq() => 0; case s => s.text.toInt }
      SIDim(L, M, T, I, Th, N, J)
    }
    val displayUnit = (source \ "unit").text
    val lower = source \ "lower" collectFirst {
      case <lower>{n}</lower> => PhysicalQuantity(n.text.toDouble, SIUnit(1d, dim))
    }
    val upper = source \ "upper" collectFirst {
      case <upper>{n}</upper> => PhysicalQuantity(n.text.toDouble, SIUnit(1d, dim))
    }
    val value = source \ "value" collectFirst {
      case <value>{n}</value> => PhysicalQuantity(n.text.toDouble, SIUnit(1d, dim))
    }
    ProtoParameter(id, name, dim, displayUnit, lower, upper, value)
  }
}
