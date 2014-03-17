package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, DoubleProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(val id: Int, x0: Double, y0: Double)(val diagram: Diagram) extends DObject(diagram.selectedConstraints)(diagram) with Draggable {

  def getConstraint(): Option[ProtoConstraint] = diagram.world().getConstraintById(id)

  // constants
  val RECT_COLOR = Color.rgb(160, 255, 160)
  val RECT_HOVER_COLOR =  Color.rgb(230, 255, 230)

  val STROKE_COLOR = Color.BLACK
  val STROKE_SELECTED_COLOR = Color.RED
  val STROKE_WIDTH = 1
  val STROKE_SELECTED_WIDTH = 2

  val REL_COLOR = Color.BLACK
  val UNDEFINED_COLOR = Color.GRAY

  val REL_MARGIN_X = 8
  val REL_MARGIN_Y = 3

  val UNDEFINED_SEQ: Seq[java.lang.Double] = Seq(4, 4)

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)

  val isDefined = BooleanProperty(false)

  private val relText = new Text() {
    x <== DConstraint.this.x
    y <== DConstraint.this.y
    stroke <== when (isDefined) choose REL_COLOR otherwise UNDEFINED_COLOR
    mouseTransparent = true

    boundsInLocal onChange {
      translateX = -boundsInLocal().width / 2
    }
  }

  private val rectangle = makeDraggable(
    makeSelectable(
      new Rectangle() {
        x <== DConstraint.this.x
        y <== DConstraint.this.y

        stroke <== when (selected) choose STROKE_SELECTED_COLOR otherwise STROKE_COLOR
        strokeWidth <== when(selected) choose STROKE_SELECTED_WIDTH otherwise STROKE_WIDTH
        fill <== when (hover) choose RECT_HOVER_COLOR otherwise RECT_COLOR

        handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
          // show the information of the constraint
          diagram.infoObject() = Some(DConstraint.this)

          me.consume()
        }

        // Resize the rectangle when the text changes.
        relText.boundsInLocal onChange {
          width = relText.boundsInLocal().width + REL_MARGIN_X * 2
          height = relText.boundsInLocal().height + REL_MARGIN_Y * 2
        }

        boundsInLocal onChange {
          translateX = -width() / 2
          translateY = -height() / 2
          relText.translateY = height() / 2 - REL_MARGIN_Y - 3
        }
      }
    ))

  // synchronize constraint with /diagram.world()/
  diagram.world onChange {
    update()
  }

  override val group = new Group(rectangle, relText)

  // update appearance of the constraint
  def update() {
    getConstraint() match {
      case None =>  // constraint no longer exists
      case Some(pc) => {
        // avoid empty string being used in order to ensure the height is enough
        relText.text = if (pc.relStr == "") " " else pc.relStr

        isDefined() = pc.rel.isDefined

        rectangle.strokeDashArray = if (isDefined()) null else UNDEFINED_SEQ
      }
    }
  }

  def toXML(): xml.Elem = {
    def vToNode(pc: ProtoConstraint, v: String) = {
      diagram.dVariables().collectFirst {
        case ((cId, varName), dv) if cId == id && varName == v => dv
      } match {
        case Some(dv) => {
          <var>
            <name>{v}</name>
            {pc.paramMap.get(v) match {
              case None =>
              case Some(pId) => <pid>{pId}</pid>
            }}
            <x>{dv.tx()}</x>
            <y>{dv.ty()}</y>
          </var>
        }
        case None => throw new NoSuchElementException(
          s"Unknown variable $v in ${pc}.")
      }
    }

    getConstraint() match {
      case None => throw new IllegalStateException(
        s"Constraint(id=$id) does not exist.")
      case Some(c) => {
        <constraint>
          <id>{id}</id>
          <rel>{c.relStr}</rel>
          {c.vars match {
            case None =>
            case Some(vs) => <vars>{ for (v <- vs) yield vToNode(c, v) }</vars>
          }}
          <x>{x()}</x>
          <y>{y()}</y>
        </constraint>
      }
    }

  }

  // initialization
  update()
}
