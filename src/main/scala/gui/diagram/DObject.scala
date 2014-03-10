package moira.gui.diagram

import scalafx.scene.Group
import scalafx.beans.property.{ObjectProperty, BooleanProperty, DoubleProperty}
import scalafx.scene.shape.Shape

import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

abstract class DObject(selectedSet: ObjectProperty[Set[DObject]])(implicit diagram: Diagram) {

  // actual node
  val group: Group

  // properties
  protected val selected = BooleanProperty(false)

  // Update selected
  selectedSet.onChange {
    selected() = selectedSet().contains(this)
  }

  protected def makeSelectable[T <: Shape](node: T): T = {
    val handler = new EventHandler[MouseEvent] {
      def handle(me: MouseEvent) {
        // reset selection if shift is not down
        if (me.isShiftDown) {
          selectedSet() += DObject.this
        } else {
          diagram.unselect()  // unselect other objects
          selectedSet() = Set(DObject.this)
        }
        me.consume()
      }
    }
    node.addEventHandler(MouseEvent.MOUSE_PRESSED, handler)
    node
  }

}
