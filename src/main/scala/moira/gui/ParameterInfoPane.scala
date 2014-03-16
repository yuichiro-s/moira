package moira.gui

import moira.unit.{SIUnit, PhysicalQuantity, CommonDims, CommonUnits}
import scalafx.beans.property.IntegerProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.{Label,TextField,ComboBox}
import scalafx.scene.layout.{HBox,VBox}

import moira.gui.diagram.Diagram
import moira.world.World

class ParameterInfoPane()(implicit diagram: Diagram) extends VBox {
  val pId = IntegerProperty(-1) // initialized with meaningless value

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
    val world: World = diagram.world()
    world.getParameterById(pId()) match {
      case None =>
      case Some(pp) => {
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
        val unit: SIUnit = CommonUnits.nameToUnit.get(newDisplayUnit) match {
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

        val (newWorld, _) = world.updateParameter(
          pp.id, newName, newDim, newDisplayUnit,
          newLower, newUpper, newValue)

        // update world
        diagram.world() = newWorld
      }
    }
  }

  // update Controls
  def updateControls() {
    diagram.world().getParameterById(pId()) match {
      case None =>
      case Some(pp) => {
        // update nameField
        nameField.text = pp.name

        // update dimBox
        dimBox.value = CommonDims.dimToName.get(pp.dim) match {
          case Some(name) => name
          case None => {
            // TODO: support dimensions not listed
            throw new UnsupportedOperationException
          }
        }

        // update unitBox
        unitBox.value = pp.displayUnit

        // update valueField, lowerField, and upperField
        def opqToStr(opq: Option[PhysicalQuantity]) = opq match {
          case Some(pq) => {
            pp.unit match {
              case Some(u) => pq.convertUnit(u).value.toString
              case None => throw new IllegalStateException(
                "%s is not a known unit.".format(pp.displayUnit))
            }
          }
          case None => ""
        }
        valueField.text = opqToStr(pp.value)
        lowerField.text = opqToStr(pp.lower)
        upperField.text = opqToStr(pp.upper)
      }
    }
  }

  diagram.world.onChange { updateControls() }

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
