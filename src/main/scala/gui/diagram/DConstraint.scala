package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.beans.property.ObjectProperty
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(pc0: ProtoConstraint, x0: Double, y0: Double)(implicit diagram: Diagram) extends Group with DObject {

  val cId = pc0.id

  // properties
  private val x = DoubleProperty(x0)
  private val y = DoubleProperty(y0)

  private val constraint = ObjectProperty(pc0)
  def getConstraint(): ProtoConstraint = constraint()
  val constraintProperty = constraint
  def setConstraint(pc: ProtoConstraint) {
    require(pc.id == cId)
    constraint() = pc
  }

  private val relText = new Text() {
    x <== DConstraint.this.x
    y <== DConstraint.this.y
    stroke = Color.BLACK
    mouseTransparent = true
  }

  private val rectangle = new Rectangle() {
    x <== DConstraint.this.x
    y <== DConstraint.this.y
    stroke = Color.DARKGREEN
    strokeWidth = 3
    fill <== when (hover) choose Color.LIGHTGREEN otherwise Color.GREEN

    handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
      // show the information of the constraint
      diagram.infoObject() = Some(DConstraint.this)
      me.consume()
    }

    // Resize the rectangle when the text changes.
    relText.boundsInLocalProperty onChange {
      width = relText.boundsInLocal().width
      height = relText.boundsInLocal().height
    }
  }


  // update appearance of the constraint
  def update() {
    val c = getConstraint()
    relText.text = c.relStr
  }

  // initialization
  update()

  // Update appearance when the constraint is changed.
  constraint.onChange(update)

  content = Seq(rectangle, relText)

}
