package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty,ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.{Circle,Rectangle}
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(pc0: ProtoConstraint, x0: Double, y0: Double)(implicit diagram: Diagram) extends DObject(diagram.selectedConstraints) {

  val cId = pc0.id

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)

  val constraint = ObjectProperty(pc0)
  def setConstraint(pc: ProtoConstraint) {
    require(pc.id == cId)
    constraint() = pc
  }

  private val relText = new Text() {
    x <== DConstraint.this.x
    y <== DConstraint.this.y
    stroke = Color.BLACK
    mouseTransparent = true
  }

  private val rectangle = makeSelectable(
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
  )

  // map from variable name to corresponding DVariable
  private var dVariableMap = Map[String, DVariable]()

  private val dVariables = new Group() {
    content = Seq()
  }

  // update appearance of the constraint
  def update() {
    val c = constraint()

    // avoid empty string being used to ensure the height is enough
    relText.text = if (c.relStr == "") " " else c.relStr

    var newDVariableMap = Map[String, DVariable]()

    dVariables.content = c.vars match {
      case None => Seq()
      case Some(vs) => vs map { varName =>
        dVariableMap.get(varName) match {
          case Some(dv) => {
            newDVariableMap += varName -> dv
            dv.group
          }
          case None => {
            val dv = new DVariable(this, varName, 20d * (dVariableMap.size + newDVariableMap.size), 20d)
            newDVariableMap += varName -> dv
            dv.group
          }
        }
      }
    }

    dVariableMap = newDVariableMap
  }

  // initialization
  update()

  // Update appearance when the constraint is changed.
  constraint onChange { update() }

  override val group = new Group(rectangle, relText, dVariables)
}
