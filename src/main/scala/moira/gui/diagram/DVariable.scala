package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.beans.property.{BooleanProperty, ObjectProperty, DoubleProperty}
import scalafx.scene.shape.{Line,Circle}
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text

class DVariable(val constraint: DConstraint, val varName: String, tx0: Double, ty0: Double)(implicit val diagram: Diagram) extends DObject(diagram.selectedVariables) with Draggable {

  // constants
  val RADIUS = 14d
  val CIRCLE_COLOR = Color.rgb(255, 255, 100)
  val CIRCLE_HOVER_COLOR =  Color.rgb(255, 255, 220)

  val STROKE_COLOR = Color.BLACK
  val STROKE_SELECTED_COLOR = Color.RED
  val STROKE_WIDTH = 1
  val STROKE_SELECTED_WIDTH = 2

  val LINE_COLOR = Color.rgb(0, 0, 50)
  val LINE_WIDTH = 1

  val NAME_COLOR = Color.BLACK
  val BOUND_COLOR = Color.GRAY

  val UNBOUND_SEQ: Seq[java.lang.Double] = Seq(4, 4)

  // properties
  val x = DoubleProperty(0d)  // initialize with meaningless value
  val y = DoubleProperty(0d)  // initialize with meaningless value
  val tx = DoubleProperty(tx0)
  val ty = DoubleProperty(ty0)

  val isBound = BooleanProperty(false)

  val dBinding: ObjectProperty[Option[DBinding]] = ObjectProperty(None)

  val bindingGroup = new Group()

  private val circle = makeDraggable(
    makeSelectable(new Circle() {
      radius = RADIUS
      centerX <== DVariable.this.x
      centerY <== DVariable.this.y

      stroke <== when (selected) choose STROKE_SELECTED_COLOR otherwise STROKE_COLOR
      strokeWidth <== when(selected) choose STROKE_SELECTED_WIDTH otherwise STROKE_WIDTH
      fill <== when (hover) choose CIRCLE_HOVER_COLOR otherwise CIRCLE_COLOR

      handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
        me.consume()
      }

      strokeDashArray = UNBOUND_SEQ
      isBound onChange {
        strokeDashArray = if (isBound()) null else UNBOUND_SEQ
      }
    }))

  private val nameText = new Text() {
    stroke <== when (isBound) choose NAME_COLOR otherwise BOUND_COLOR
    x <== DVariable.this.x
    y <== DVariable.this.y
    translateY = 4
    mouseTransparent = true

    text onInvalidate {
      translateX = -boundsInLocal().width / 2
    }

    text = varName
  }

  // line which connects variable and its parent constraint
  private val constraintLine = new Line() {
    startX <== constraint.x
    startY <== constraint.y
    endX <== DVariable.this.x
    endY <== DVariable.this.y

    stroke = LINE_COLOR
    strokeWidth = LINE_WIDTH
  }

  x <== constraint.x + tx
  y <== constraint.y + ty

  dBinding onInvalidate {
    bindingGroup.content = dBinding().toSeq.map(_.group)
  }

  override val group = new Group(constraintLine, bindingGroup, circle, nameText)
}
