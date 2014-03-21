package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty,DoubleProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Circle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoParameter

class DParameter(val id: Int, x0: Double, y0: Double)(val diagram: Diagram) extends DObject(diagram.selectedParameters)(diagram) with Draggable {

  def getParameter(): Option[ProtoParameter] = diagram.world().getParameterById(id)

  // constants
  val RADIUS = 25d
  val CIRCLE_COLOR = Color.rgb(255, 160, 160)
  val CIRCLE_HOVER_COLOR =  Color.rgb(255, 200, 200)

  val STROKE_COLOR = Color.BLACK
  val STROKE_SELECTED_COLOR = Color.RED
  val STROKE_WIDTH = 1
  val STROKE_SELECTED_WIDTH = 2

  val NAME_COLOR = Color.BLACK
  val VALUE_COLOR = Color.BLACK
  val BOUND_COLOR = Color.GRAY

  val UNBOUND_SEQ: Seq[java.lang.Double] = Seq(4, 4)

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)
  val isBound = BooleanProperty(false)

  private val circle: Circle = makeDraggable(
    makeSelectable(
      new Circle() {
        radius = RADIUS
        centerX <== DParameter.this.x
        centerY <== DParameter.this.y
        stroke <== when (selected) choose STROKE_SELECTED_COLOR otherwise STROKE_COLOR
        strokeWidth <== when(selected) choose STROKE_SELECTED_WIDTH otherwise STROKE_WIDTH
        fill <== when (hover) choose CIRCLE_HOVER_COLOR otherwise CIRCLE_COLOR

        handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
          // show the information of the parameter
          diagram.infoObject() = Some(DParameter.this)

          me.consume()
        }
      }))

  private val nameText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    translateY = -4
    stroke = NAME_COLOR
    mouseTransparent = true

    text.onChange {
      translateX = -boundsInLocal().width / 2
    }
  }

  private val valueText = new Text() {
    x <== DParameter.this.x
    y <== DParameter.this.y
    translateY = 16
    stroke <== when (isBound) choose VALUE_COLOR otherwise BOUND_COLOR
    mouseTransparent = true

    text.onChange {
      translateX = -boundsInLocal().width / 2
    }
  }

  // update appearance of the parameter
  def update() {
    getParameter() match {
      case None => // parameter no longer exists
      case Some(pp) => {
        isBound() = pp.value.isDefined
        nameText.text = pp.name
        valueText.text = pp.value match {
          case Some(pq) => f"${pq.value}%g ${pp.displayUnit}"
          case None => {
            // When the parameter is not bound, show its dimension.
            pp.dim.toString
          }
        }

        circle.strokeDashArray = if (isBound()) null else UNBOUND_SEQ
      }
    }
  }

  // synchronize parameter with /diagram.world/
  diagram.world onChange { update() }

  override val group = new Group(circle, nameText, valueText)

  def toXML(): xml.Elem = {
    getParameter() match {
      case None => throw new IllegalStateException(
        s"Parameter(id=$id) does not exist.")
      case Some(p) => {
        val dimXML = {
          val d = p.dim
          <dim>
            {if (d.m != 0) <L>{d.m}</L>}
            {if (d.kg != 0) <M>{d.kg}</M>}
            {if (d.s != 0) <T>{d.s}</T>}
            {if (d.a != 0) <I>{d.a}</I>}
            {if (d.k != 0) <Th>{d.k}</Th>}
            {if (d.cd != 0) <N>{d.cd}</N>}
            {if (d.mol != 0) <J>{d.mol}</J>}
          </dim>
        }

        <parameter>
          <id>{p.id}</id>
          <name>{p.name}</name>
          {dimXML}
          <unit>{p.displayUnit}</unit>
          {p.lower match { case None =>; case Some(pq) => <lower>{pq.normalized.value}</lower> }}
          {p.upper match { case None =>; case Some(pq) => <upper>{pq.normalized.value}</upper> }}
          {p.value match { case None =>; case Some(pq) => <value>{pq.normalized.value}</value> }}
          <x>{x()}</x>
          <y>{y()}</y>
        </parameter>

      }
    }
  }

  // initialization
  update()
}

