package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.beans.property.DoubleProperty
import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text

class DVariable(constraint: DConstraint, varName: String, tx0: Double, ty0: Double)(implicit diagram: Diagram) extends DObject(diagram.selectedVariables) {

  // properties
  private val x = DoubleProperty(0d)  // initialize with meaningless value
  private val y = DoubleProperty(0d)  // initialize with meaningless value
  private val tx = DoubleProperty(tx0)
  private val ty = DoubleProperty(ty0)

  private val circle = makeSelectable(new Circle() {
    radius = 10d
    stroke = Color.BLACK
    strokeWidth <== when(selected) choose 4 otherwise 1
    centerX <== DVariable.this.x
    centerY <== DVariable.this.y

    fill <== when (hover) choose Color.LIGHTSALMON otherwise Color.LIGHTBLUE

    handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
      me.consume()
    }
  })

  private val nameText = new Text() {
    text = varName
    stroke = Color.BLACK
    x <== DVariable.this.x
    y <== DVariable.this.y
    mouseTransparent = true
  }

  x <== constraint.x + tx
  y <== constraint.y + ty

  override val group = new Group(circle, nameText)
}
