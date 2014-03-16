package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty,ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(val id: Int, x0: Double, y0: Double)(val diagram: Diagram) extends DObject(diagram.selectedConstraints)(diagram) with Draggable {

  def getConstraint(): Option[ProtoConstraint] = diagram.world().getConstraintById(id)

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)
  val centerX = DoubleProperty(x0)
  val centerY = DoubleProperty(y0)

  private val relText = new Text() {
    x <== DConstraint.this.x
    y <== DConstraint.this.y
    stroke = Color.BLACK
    mouseTransparent = true
  }

  private val rectangle = makeDraggable(
    makeSelectable(
      new Rectangle() {
        x <== DConstraint.this.x
        y <== DConstraint.this.y
        stroke = Color.TOMATO
        strokeWidth <== when(selected) choose 4 otherwise 2
        fill <== when (hover) choose Color.LIGHTGREEN otherwise Color.GREEN

        handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
          // show the information of the constraint
          diagram.infoObject() = Some(DConstraint.this)

          me.consume()
        }

        // Resize the rectangle when the text changes.
        relText.boundsInLocalProperty onChange {
          width = relText.boundsInLocal().width
          height = relText.boundsInLocal().height
        }
      }
    ))

  centerX <== x + rectangle.width / 2
  centerY <== y + rectangle.height / 2

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
