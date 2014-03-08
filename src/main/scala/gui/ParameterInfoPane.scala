package moira.gui

import moira.unit.PhysicalQuantity
import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.{Label,TextField,ComboBox}
import scalafx.scene.layout.{HBox,VBox}

import moira.gui.diagram.{Diagram,DParameter}
import moira.world.{World,ProtoParameter}
import moira.unit.{CommonDims,CommonUnits}

class ParameterInfoPane()(implicit diagram: Diagram) extends VBox {
  val pp = ObjectProperty(ProtoParameter())

  // controllers
  // User input is validated here, and if valid, the change takes place.
  val nameField = new TextField()

  val dimBox = new ComboBox[String]() {
    items = ObservableBuffer(CommonDims.nameToDim.keys.toSeq)
  }

  val unitBox = new ComboBox[String]() {
    // When /dimBox/ is changed, /unitBox/ updates its entries
    // to show the corresponding unit names.
    dimBox.value onChange { (_, _, dimStr) =>
      CommonDims.nameToDim.get(dimStr) match {
        case None => new IllegalStateException("/dimBox.value()/ must be a name of a known dimension.")
        case Some(dim) => {
          // update unitBox
          items = ObservableBuffer(CommonUnits.unitNames(dim).map(_._1))
        }
      }
    }
  }

  val valueField = new TextField()
  val lowerField = new TextField()
  val upperField = new TextField()

  val valueUnit = new Label() { text <== unitBox.value }
  val lowerUnit = new Label() { text <== unitBox.value }
  val upperUnit = new Label() { text <== unitBox.value }

  def updateParameter() {
    val oldWorld: World = diagram.world()
    val oldParam: ProtoParameter = pp()

    // create new name
    val newName = nameField.text()

    // create new dim
    val newDim = CommonDims.nameToDim.get(dimBox.value()) match {
      case Some(dim) => dim
      case _ => throw new IllegalStateException(
        "%s is not a known dimension.".format(dimBox.value()))
    }

    // create new displayUnit
    val newDisplayUnit = unitBox.value()

    // create new lower, upper, value
    val unit = CommonUnits.nameToUnit.get(newDisplayUnit) match {
      case Some(u) => u
      case None => throw new IllegalStateException(
        "%s is not a known unit.".format(newDisplayUnit))
    }
    // If /s/ cannot be parsed into a /Double/, returns /None/.
    def createPQ(s: String): Option[PhysicalQuantity] = {
      try {
        Some(PhysicalQuantity(s.toDouble, unit))
      } catch {
        case _: NumberFormatException => None
      }
    }
    val newLower = createPQ(lowerField.text())
    val newUpper = createPQ(upperField.text())
    val newValue = createPQ(valueField.text())

    val (newWorld, newParam) = oldWorld.updateParameter(
      oldParam.id, newName, newDim, newDisplayUnit,
      newLower, newUpper, newValue)

    // update parameter
    diagram.infoObject() match {
      case Some(dp: DParameter) => dp.setParameter(newParam)
      case _ => throw new IllegalStateException("DParameter is not selected.")
    }

    // update world
    diagram.world() = newWorld
  }

  // update Controls
  def updateControls() {
    val p = pp()

    // update nameField
    nameField.text = p.name

    // update dimBox
    dimBox.value = CommonDims.dimToName.get(p.dim) match {
      case Some(name) => name
      case None => {
        // TODO: support dimensions not listed
        throw new UnsupportedOperationException
      }
    }

    // update unitBox
    unitBox.value = p.displayUnit

    // update valueField, lowerField, and upperField
    def opqToStr(opq: Option[PhysicalQuantity]) = opq match {
      case Some(pq) => pq.value.toString
      case None => ""
    }
    valueField.text = opqToStr(p.value)
    lowerField.text = opqToStr(p.lower)
    upperField.text = opqToStr(p.upper)

    //valueUnit.text = p.displayUnit
    //lowerUnit.text = p.displayUnit
    //upperUnit.text = p.displayUnit
  }

  pp.onChange { updateControls() }

  content = Seq(
    new Label("Parameter"),
    createItem("Name", nameField),
    createItem("Dim", dimBox),
    createItem("Unit", unitBox),
    createItem("Value", valueField, valueUnit),
    createItem("lower", lowerField, lowerUnit),
    createItem("upper", upperField, upperUnit)
  )

  private def createItem(name: String, item: Node*): HBox = {
    new HBox() {
      content = Seq(new Label(name)) ++ item
    }
  }
}
