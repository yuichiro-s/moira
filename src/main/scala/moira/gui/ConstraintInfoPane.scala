package moira.gui


import moira.expression.Parser
import scalafx.beans.property.IntegerProperty
import scalafx.scene.control.{Label,TextField}
import scalafx.scene.layout.{HBox,VBox}

import moira.gui.diagram.Diagram
import moira.world.World

class ConstraintInfoPane()(implicit diagram: Diagram) extends VBox {
  val cId = IntegerProperty(-1) // initialized with meaningless value

  // controllers
  // User input is validated here, and if valid, the change takes place.
  val relField = new TextField()

  def updateConstraint() {
    val world: World = diagram.world()
    world.getConstraintById(cId()) match {
      case None =>
      case Some(pc) => {
        // create new name
        val newRel = relField.text()

        // remove binding for variables which do not appear in /rel/
        val newParamMap: Map[String, Int] = {
          Parser.parseRel(newRel) match {
            case None => Map()
            case Some(rel) => {
              pc.paramMap.filter {
                case (varName, _) => rel.vars.contains(varName)
              }
            }
          }
        }

        val (newWorld, _) = world.updateConstraint(pc.id, newRel, newParamMap)

        // update world
        diagram.world() = newWorld
      }
    }
  }

  // update Controls
  def updateControls() {
    diagram.world().getConstraintById(cId()) match {
      case None =>
      case Some(pc) => {
        // update relField
        relField.text = pc.relStr
      }
    }
  }

  diagram.world.onChange { updateControls() }

  content = Seq(
    new Label("Constraint"),
    new HBox() { content = Seq(new Label("Relation"), relField) }
  )
}
