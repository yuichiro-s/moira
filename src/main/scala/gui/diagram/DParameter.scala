package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoParameter

class DParameter(pp0: ProtoParameter, x0: Double, y0: Double)(implicit diagram: Diagram) extends DObject(diagram.selectedParameters) {

  val pId = pp0.id

  // constants
  val RADIUS = 20d

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)

  private val parameter = ObjectProperty(pp0)
  def getParameter() = parameter()
  val parameterProperty = parameter

  private val circle: Circle = makeSelectable(
    new Circle() {
      radius = RADIUS
      centerX <== DParameter.this.x
      centerY <== DParameter.this.y
      stroke = Color.RED
      strokeWidth <== when(selected) choose 4 otherwise 2
      fill <== when (hover) choose Color.LIGHTYELLOW otherwise Color.LIGHTPINK

      handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
        // show the information of the parameter
        diagram.infoObject() = Some(DParameter.this)

        me.consume()
      }
    }
  )

  private val nameText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    stroke = Color.BLUE
    mouseTransparent = true
  }

  private val valueText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    translateY = 20
    stroke = Color.GREEN
    mouseTransparent = true
  }

  // update appearance of the parameter
  def update() {
    val p = parameter()

    nameText.text = p.name
    valueText.text = p.value match {
      case Some(pq) =>  pq.toString
      case None => {
        // When the parameter is not bound, show its dimension.
        p.dim.toString
      }
    }
  }

  // Update appearance when the parameter is changed.
  parameter.onChange(update)

  // synchronize parameter with /diagram.world/
  diagram.world onChange {
    val pp = diagram.world().getParameterById(pId)
    parameter() = pp match {
      case Some(pp) => pp
      case None => {
        // This parameter has been removed.
        parameter() // This return value is meaningless.
      }
    }
  }

  override val group = new Group(circle, nameText, valueText)

  // initialization
  update()
}
