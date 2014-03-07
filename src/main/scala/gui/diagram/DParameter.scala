package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty
import scalafx.beans.property.ObjectProperty
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoParameter

class DParameter(pp0: ProtoParameter, x0: Double, y0: Double)(implicit diagram: Diagram) extends Group with DObject {

  // constants
  val RADIUS = 20d

  val pId = pp0.id

  // properties
  private val x = DoubleProperty(x0)
  private val y = DoubleProperty(y0)
  private val protoParameter = ObjectProperty(pp0)

  private val circle = new Circle() {
    radius = RADIUS
    centerX <== DParameter.this.x
    centerY <== DParameter.this.y
    stroke = Color.RED
    strokeWidth = 3
    fill <== when (hover) choose Color.GREEN otherwise Color.PINK

    handleEvent(MouseEvent.MousePressed) {
      // show the information of the parameter
      diagram.infoObject() = Some(DParameter.this)
    }
  }

  private val nameText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    text = protoParameter().name
    stroke = Color.BLUE
    mouseTransparent = true
  }

  private val valueText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    text = protoParameter().displayUnit.toString
    translateY = 20
    stroke = Color.GREEN
    mouseTransparent = true
  }

  content = Seq(circle, nameText, valueText)

  def getProtoParameter(): ProtoParameter = protoParameter()
  val protoParameterProperty = protoParameter
  def setProtoParameter(pp: ProtoParameter) {
    require(pp.id == pId)
    protoParameter() = pp
  }
}
