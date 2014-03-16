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

  private val line = makeSelectable(new Line() {
    stroke <== when (hover) choose Color.LIGHTGRAY otherwise Color.INDIGO
    strokeWidth <== when (selected) choose 4 otherwise 2

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
