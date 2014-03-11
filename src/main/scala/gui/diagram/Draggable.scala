package moira.gui.diagram

import scalafx.beans.property.ObjectProperty
import scalafx.scene.shape.Shape
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

trait Draggable {
  val diagram: Diagram

  case class DragInfo(x0: Double, y0: Double)
  val dragInfo = ObjectProperty(DragInfo(0d, 0d))

  def setDragInfo(x: Double, y: Double) {
    dragInfo() = DragInfo(x, y)
  }

  // Makes the shape draggable.
  // /x/ and /y/ change following the change in its position.
  protected def makeDraggable[S <: Shape](node: S): S = {
    val handler = new EventHandler[MouseEvent] {
      def handle(me: MouseEvent) {
        me.getEventType() match {
          case MouseEvent.MOUSE_PRESSED => diagram.startDrag(me.getX(), me.getY())
          case MouseEvent.MOUSE_DRAGGED => diagram.drag(me.getX(), me.getY())
          case _ =>
        }
        me.consume()
      }
    }

    node.addEventHandler(MouseEvent.ANY, handler)
    node
  }

}

