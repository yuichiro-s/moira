package moira.gui.diagram

import scalafx.scene.Group
import scalafx.beans.property.{ObjectProperty,BooleanProperty}
import scalafx.scene.shape.Shape

import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

abstract class DObject[T](selectedSet: ObjectProperty[Set[T]])(implicit diagram: Diagram) {

  type Id = T
  val id: Id

  // actual node
  val group: Group

  // properties
  protected val selected = BooleanProperty(false)

  // Update selected
  selectedSet.onChange {
    selected() = selectedSet().contains(id)
  }

  // Makes the shape selectable.
  protected def makeSelectable[S <: Shape](node: S): S = {
    val handler = new EventHandler[MouseEvent] {
      def handle(me: MouseEvent) {
        // reset selection if shift is not down
        if (!me.isShiftDown) {
          diagram.unselect()  // unselect other objects
        }
        selectedSet() += id
        me.consume()
      }
    }

    node.addEventHandler(MouseEvent.MOUSE_PRESSED, handler)
    node
  }

}
