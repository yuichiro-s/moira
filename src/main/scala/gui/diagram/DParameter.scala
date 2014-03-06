package moira.gui.diagram

import scalafx.Includes._
import scalafx.scene.text.Text
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.scene.paint.Color

class DParameter(val paramId: Int, var x0: Double, var y0: Double)(implicit val diagram: Diagram) extends Group {

  val RADIUS = 20d

  val circle = new Circle() {
    radius = RADIUS
    centerX = x0
    centerY = y0
    stroke = Color.RED
    strokeWidth = 3
    fill <== when (hover) choose {
      Color.GREEN
    } otherwise {
      Color.PINK
    }
  }

  val nameText = new Text("hello") {
    x = x0
    y = y0
    stroke = Color.BLUE
    mouseTransparent = true
  }

  val valueText = new Text("3.0 m/s") {
    x = x0
    y = y0 + 20
    stroke = Color.GREEN
    mouseTransparent = true
  }

  content = Seq(circle, nameText, valueText)
}
