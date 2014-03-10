package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.beans.property.{ObjectProperty, DoubleProperty}
import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text

class DVariable(val constraint: DConstraint, val varName: String, tx0: Double, ty0: Double)(implicit diagram: Diagram) extends DObject(diagram.selectedVariables) {

  // properties
  val x = DoubleProperty(0d)  // initialize with meaningless value
  val y = DoubleProperty(0d)  // initialize with meaningless value
  val tx = DoubleProperty(tx0)
  val ty = DoubleProperty(ty0)

  val dBinding: ObjectProperty[Option[DBinding]] = ObjectProperty(None)

  val bindingGroup = new Group()

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

  dBinding onInvalidate {
    bindingGroup.content = dBinding().toSeq.map(_.group)
  }

  override val group = new Group(circle, nameText, bindingGroup)
}
