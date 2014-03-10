package moira.gui


import moira.expression.Parser
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{Label,TextField}
import scalafx.scene.layout.{HBox,VBox}

import moira.gui.diagram.{DConstraint, Diagram}
import moira.world.{ProtoParameter, World, ProtoConstraint}

class ConstraintInfoPane()(implicit diagram: Diagram) extends VBox {
  val pc = ObjectProperty(ProtoConstraint())

  // controllers
  // User input is validated here, and if valid, the change takes place.
  val relField = new TextField()

  def updateConstraint() {
    val oldWorld: World = diagram.world()
    val oldConst: ProtoConstraint = pc()

    // create new name
    val newRel = relField.text()

    // remove binding for variables which do not appear in /rel/
    val newParamMap: Map[String, ProtoParameter] = {
      Parser.parseRel(newRel) match {
        case Some(rel) => {
          oldConst.paramMap.filter {
            case (varName, _) => rel.vars.contains(varName)
          }
        }
        case None => Map()
      }
    }

    val (newWorld, _) = oldWorld.updateConstraint(
      oldConst.id, newRel, newParamMap)

    // update world
    diagram.world() = newWorld
  }

  // update Controls
  def updateControls() {
    val p = pc()

    // update relField
    relField.text = p.relStr
  }

  pc.onChange { updateControls() }

  content = Seq(
    new Label("Constraint"),
    new HBox() { content = Seq(new Label("Relation"), relField) }
  )
}
