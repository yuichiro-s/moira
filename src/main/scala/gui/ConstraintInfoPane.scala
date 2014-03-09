package moira.gui


import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{Label,TextField}
import scalafx.scene.layout.{HBox,VBox}

import moira.unit.PhysicalQuantity
import moira.gui.diagram.{DConstraint, Diagram}
import moira.world.{World, ProtoParameter, ProtoConstraint}
import moira.unit.{CommonDims,CommonUnits}

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

    val (newWorld, newConst) = oldWorld.updateConstraint(
      oldConst.id, newRel, oldConst.paramMap)

    // update constraint
    diagram.infoObject() match {
      case Some(dc: DConstraint) => dc.setConstraint(newConst)
      case _ => throw new IllegalStateException("DConstraint is not selected.")
    }

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
