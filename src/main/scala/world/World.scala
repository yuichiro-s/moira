package moira.world

// 
case class World(
  val constraints: Set[ProtoConstraint],
  val parameters: Set[ProtoParameter]
) {
  //def addConstraint
  //def updateConstraint
  //def removeConstraint
  //def addParameter
  //def updateParameter
  //def removeParameter

  lazy val toXML: xml.Elem = {

  }
}
