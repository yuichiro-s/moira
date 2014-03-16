package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.shape.Line
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent

class DBinding(val vId: (Int, String), val pId: Int)(diagram: Diagram) extends DObject(diagram.selectedBindings)(diagram) {

  val cId = vId._1
  val varName = vId._2

  override val id = (vId, pId)

  // constants
  val STROKE_COLOR = Color.rgb(0, 0, 200)
  val STROKE_HOVER_COLOR = Color.rgb(150, 150, 255)
  val STROKE_SELECTED_COLOR = Color.RED
  val STROKE_WIDTH = 3
  val STROKE_SELECTED_WIDTH = 4

  private val line = makeSelectable(new Line() {
    stroke <== when (selected) choose STROKE_SELECTED_COLOR otherwise (when (hover) choose STROKE_HOVER_COLOR otherwise STROKE_COLOR)
    strokeWidth <== when(selected) choose STROKE_SELECTED_WIDTH otherwise STROKE_WIDTH

    handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
      me.consume()
    }
  })

  override val group = new Group(line)

  def update() {
    (diagram.dParameters().get(pId), diagram.dVariables().get(vId)) match {
      case (Some(dp), Some(dv)) => {
        // rebind properties of line
        line.startX <== dv.x
        line.startY <== dv.y
        line.endX <== dp.x
        line.endY <== dp.y
      }
      case _ => // either parameter or variable no longer exists
    }
  }

  diagram.world onChange { update() }

  // initialization
  update()
}
