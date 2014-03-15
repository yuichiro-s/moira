package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.shape.Line
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent

class DBinding(val variable: DVariable, val parameter: DParameter)(implicit diagram: Diagram) extends DObject(diagram.selectedBindings) {

  // constants
  val STROKE_COLOR = Color.rgb(0, 0, 200)
  val STROKE_HOVER_COLOR = Color.rgb(150, 150, 255)
  val STROKE_SELECTED_COLOR = Color.RED
  val STROKE_WIDTH = 3
  val STROKE_SELECTED_WIDTH = 4

  private val line = makeSelectable(new Line() {
    stroke <== when (selected) choose STROKE_SELECTED_COLOR otherwise (when (hover) choose STROKE_HOVER_COLOR otherwise STROKE_COLOR)
    strokeWidth <== when(selected) choose STROKE_SELECTED_WIDTH otherwise STROKE_WIDTH

    // start point is variable
    startX <== variable.x
    startY <== variable.y

    // end point is parameter
    endX <== parameter.x
    endY <== parameter.y

    handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
      me.consume()
    }
  })

  override val group = new Group(line)
}
