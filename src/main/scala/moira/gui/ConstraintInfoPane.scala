package moira.gui

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.scene.control.{Label,TextField}
import scalafx.scene.layout.{HBox,VBox}

import moira.expression.Parser
import moira.gui.diagram.Diagram
import moira.world.ProtoConstraint

class ConstraintInfoPane()(implicit diagram: Diagram) extends VBox {
  // keep track of which constraint is currently shown
  val cId: ObjectProperty[Option[Int]] = ObjectProperty(None)

  // Whether all controls have been initialized.
  val initialized: BooleanProperty = BooleanProperty(false)

  // controls
  // User input is validated here, and if valid, the change takes place.
  val relField = new TextField() {
    onAction = handle { updateConstraint() }
  }

  // Returns currently shown /ProtoConstraint/
  def getConstraint(): Option[ProtoConstraint] = {
    cId() flatMap { id =>
      diagram.world().getConstraintById(id)
    }
  }

  // update constraint in /world/ based on input information
  def updateConstraint() {
    // Do not update constraint in /world/ until all controls are updated.
    if (!initialized()) {
      return
    }

    getConstraint() match {
      case None =>
      case Some(oldPc) => {
        // create new name
        val newRel = relField.text()

        // remove binding for variables which do not appear in /rel/
        val newParamMap: Map[String, Int] = {
          Parser.parseRel(newRel) match {
            case None => Map()
            case Some(rel) => {
              oldPc.paramMap.filter {
                case (varName, _) => rel.vars.contains(varName)
              }
            }
          }
        }

        val (newWorld, newPc) = diagram.world().updateConstraint(oldPc.id, newRel, newParamMap)

        // update world only when /newPc/ is different from /oldPc/
        // Note that infinite loop will take place without this.
        if (oldPc != newPc) {
          // update world
          diagram.world() = newWorld
        }
      }
    }
  }

  // update Controls
  def updateControls() {
    getConstraint() match {
      case None =>
      case Some(pc) => {
        // Prohibit execution of /updateConstraint/ until all controls are updated.
        initialized() = false

        // update relField
        relField.text = pc.relStr

        // Allow execution of /updateConstraint/ again.
        initialized() = true
      }
    }
  }

  diagram.world onChange {
    updateControls()
  }

  // Update controls when /pId/ is changed.
  cId onChange {
    updateControls()
  }

  content = Seq(
    new Label("Constraint"),
    new HBox() { content = Seq(new Label("Relation"), relField) }
  )
}
