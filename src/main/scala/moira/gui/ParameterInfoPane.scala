package moira.gui

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.{Label,TextField,ComboBox}
import scalafx.scene.layout.{HBox,VBox}

import moira.unit.{SIUnit, PhysicalQuantity, CommonDims, CommonUnits}
import moira.gui.diagram.Diagram
import moira.world.ProtoParameter

class ParameterInfoPane()(implicit diagram: Diagram) extends VBox {
  // keep track of which parameter is currently shown
  val pId: ObjectProperty[Option[Int]] = ObjectProperty(None)

  // Whether all controls have been initialized.
  val initialized: BooleanProperty = BooleanProperty(false)

  // properties
  val unit: ObjectProperty[Option[SIUnit]] = ObjectProperty(None)

  // controls
  // User input is validated here, and if valid, the change takes place.
  val nameField = new TextField() {
    onAction = handle { updateParameter() }
  }

  // list of dimensions
  val dimBox = new ComboBox[String]() {
    items = ObservableBuffer(CommonDims.nameToDim.keys.toSeq)
  }

  // list of units which have the specified dimension
  val unitBox = new ComboBox[String]() {
    // When /dimBox/ is changed, /unitBox/ updates its entries
    // to show the corresponding unit names.
    dimBox.value onChange { (_, _, dimStr) =>
      CommonDims.nameToDim.get(dimStr) match {
        case None => new IllegalStateException(
          s"$dimStr is not a known dimension.")
        case Some(dim) => {
          val unitNames = CommonUnits.unitNames(dim).map(_._1)

          assert(unitNames.nonEmpty,
            s"There is no registered unit of dimension $dim.")

          // update /unitBox/
          items = ObservableBuffer(unitNames)

          // select first element
          value = unitNames(0)
        }
      }
    }

    value onChange { (_, _, unitStr) =>
      if (unitStr == null) {
        unit() = None
      } else {
        unit() = CommonUnits.parseUnit(unitStr) match {
          case Some(u) => Some(u)
          case None => throw new IllegalStateException(
            s"$unitStr is not a known unit.")
        }
        // nothing is selected(unitStr == null) right after /dimBox/ is changed.
        updateParameter()
      }
    }
  }

  // /TextField/ which recalculates value when accompanying unit is changed.
  class PQField extends TextField {
    unit onChange { (_, oldUnit, newUnit) =>
      (oldUnit, newUnit) match {
        case (Some(ou), Some(nu)) => {
          try {
            val oldPq = PhysicalQuantity(text().toDouble, ou)
            val newPq = oldPq.convertUnit(nu)

            text = newPq.value.toString
          } catch {
            case _: NumberFormatException =>
          }
        }
        case _ =>
      }
    }

    onAction = handle { updateParameter() }
  }

  val valueField = new PQField()
  val lowerField = new PQField()
  val upperField = new PQField()

  val valueUnit = new Label() { text <== unitBox.value }
  val lowerUnit = new Label() { text <== unitBox.value }
  val upperUnit = new Label() { text <== unitBox.value }

  // Returns currently shown /ProtoParameter/
  def getParameter(): Option[ProtoParameter] = {
    pId() flatMap { id =>
      diagram.world().getParameterById(id)
    }
  }

  def updateParameter() {
    // Do not update parameter in /world/ until all controls are updated.
    if (!initialized()) {
      return
    }

    getParameter() match {
      case None =>
      case Some(oldPp) => {
        // create new name
        val newName = nameField.text()

        // create new dim
        val newDim = CommonDims.nameToDim.get(dimBox.value()) match {
          case Some(dim) => dim
          case _ => throw new IllegalStateException(
            s"${dimBox.value()} is not a known dimension.")
        }

        // create new displayUnit
        val newDisplayUnit = unitBox.value()

        // create new lower, upper, value

        // If /s/ cannot be parsed into a /Double/, returns /None/.
        def createPQ(s: String): Option[PhysicalQuantity] = {
          try {
            unit().map { u => PhysicalQuantity(s.toDouble, u) }
          } catch {
            case _: NumberFormatException => None
          }
        }
        val newLower = createPQ(lowerField.text())
        val newUpper = createPQ(upperField.text())
        val newValue = createPQ(valueField.text())

        val (newWorld, newPp) = diagram.world().updateParameter(
          oldPp.id, newName, newDim, newDisplayUnit,
          newLower, newUpper, newValue)

        // update world only when /newPp/ is different from /oldPp/
        // Note that infinite loop will take place without this.
        if (oldPp != newPp) {
          diagram.world() = newWorld
        }
      }
    }
  }

  // update Controls
  def updateControls() {
    getParameter() match {
      case None =>
      case Some(pp) => {
        // Prohibit execution of /updateParameter/ until all controls are updated.
        initialized() = false

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
                s"${pp.displayUnit} is not a known unit.")
            }
          }
          case None => ""
        }
        valueField.text = opqToStr(pp.value)
        lowerField.text = opqToStr(pp.lower)
        upperField.text = opqToStr(pp.upper)

        // Allow execution of /updateParameter/ again.
        initialized() = true
      }
    }
  }

  // Update controls when /world/ is changed.
  diagram.world onChange {
    updateControls()
  }

  // Update controls when /pId/ is changed.
  pId onChange {
    updateControls()
  }

  content = Seq(
    new Label("Parameter"),
    createItem("Name", nameField),
    createItem("Dim", dimBox),
    createItem("Unit", unitBox),
    createItem("Value", valueField, valueUnit),
    createItem("Lower", lowerField, lowerUnit),
    createItem("Upper", upperField, upperUnit)
  )

  private def createItem(name: String, item: Node*): HBox = {
    new HBox() {
      content = Seq(new Label(name)) ++ item
    }
  }
}
