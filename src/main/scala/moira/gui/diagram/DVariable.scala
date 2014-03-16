package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.beans.property.DoubleProperty
import scalafx.scene.shape.{Line,Circle}
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text

class DVariable(val cId: Int, val varName: String, tx0: Double, ty0: Double)(val diagram: Diagram) extends DObject(diagram.selectedVariables)(diagram) with Draggable {

  override val id = (cId, varName)

  // properties
  val x = DoubleProperty(0d)  // initialize with meaningless value
  val y = DoubleProperty(0d)  // initialize with meaningless value
  val tx = DoubleProperty(tx0)
  val ty = DoubleProperty(ty0)

  private val circle = makeDraggable(
    makeSelectable(new Circle() {
      radius = 10d
      stroke = Color.BLACK
      strokeWidth <== when(selected) choose 4 otherwise 1
      centerX <== DVariable.this.x
      centerY <== DVariable.this.y

      fill <== when (hover) choose Color.LIGHTSALMON otherwise Color.LIGHTBLUE

      handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
        me.consume()
      }
    }))

  private val nameText = new Text() {
    text = varName
    stroke = Color.BLACK
    x <== DVariable.this.x
    y <== DVariable.this.y
    mouseTransparent = true
  }

  // line which connects variable and its parent constraint
  private val constraintLine = new Line() {
    endX <== DVariable.this.x
    endY <== DVariable.this.y

    stroke = Color.DARKKHAKI
    strokeWidth <== 1
  }

  override val group = new Group(constraintLine, circle, nameText)

  def update() {
    diagram.dConstraints().get(cId) match {
      case None =>  // parent constraint no longer exists
      case Some(dc) => {
        // rebind properties of the /DVariable/
        x <== dc.x + tx
        y <== dc.y + ty

        // rebind properties of /constraintLine/
        constraintLine.startX <== dc.centerX
        constraintLine.startY <== dc.centerY
      }
    }
  }

  diagram.world onChange { update() }

  // initialization
  update()
}
