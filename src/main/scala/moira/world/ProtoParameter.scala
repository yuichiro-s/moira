package moira.world

import moira.constraint.Parameter
import moira.unit.{SIDim,SIUnit,PhysicalQuantity}
import moira.unit.CommonUnits
import scala.xml.Node

class InsufficientParameterConfigException(pp: ProtoParameter, msg: String) extends RuntimeException(s"${pp.name}: $msg")

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
  val unit: Option[SIUnit] = CommonUnits.parseUnit(displayUnit)

  // dimensions must be the same
  require(unit match { case Some(u) => u.dim == dim; case None => true })
  require(lower match { case Some(pq) => pq.dim == dim; case None => true })
  require(upper match { case Some(pq) => pq.dim == dim; case None => true })
  require(value match { case Some(pq) => pq.dim == dim; case None => true })

  // make parameter
  lazy val toParameter: Parameter = {
    (lower, upper) match {
      case (None, _) => {
        throw new InsufficientParameterConfigException(this,
          s"Lower bound is not defined.")
      }
      case (_, None) => {
        throw new InsufficientParameterConfigException(this,
          s"Upper bound is not defined.")
      }
      case (Some(pqL), Some(pqU)) => {
        if ((pqU - pqL).value >= 0) {
          Parameter(id, dim, pqL, pqU)
        } else {
          throw new InsufficientParameterConfigException(this,
            s"Upper bound ${pqU} is smaller than lower bound ${pqL}.")
        }
      }
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
