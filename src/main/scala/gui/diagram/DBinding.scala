package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.shape.Line
import scalafx.scene.paint.Color
import scalafx.scene.input.MouseEvent

class DBinding(val variable: DVariable, val parameter: DParameter)(implicit diagram: Diagram) extends DObject(diagram.selectedBindings) {

  private val line = makeSelectable(new Line() {
    stroke <== when (hover) choose Color.LIGHTGRAY otherwise Color.INDIGO
    strokeWidth <== when (selected) choose 4 otherwise 2

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
