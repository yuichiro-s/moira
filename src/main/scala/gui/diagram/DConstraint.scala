package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty,ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.{Circle,Rectangle}
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(pc0: ProtoConstraint, x0: Double, y0: Double)(implicit diagram: Diagram) extends DObject(x0, y0, diagram.selectedConstraints) {

  val cId = pc0.id

  // properties
  private val constraint = ObjectProperty(pc0)
  def getConstraint(): ProtoConstraint = constraint()
  val constraintProperty = constraint
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
      stroke = Color.GREEN
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

  class DVariable(varName: String, tx0: Double, ty0: Double) extends Group {

    // properties
    private val x = DoubleProperty(0d)  // initialize with meaningless value
    private val y = DoubleProperty(0d)  // initialize with meaningless value
    private val tx = DoubleProperty(tx0)
    private val ty = DoubleProperty(ty0)

    private val circle = new Circle() {
      radius = 10d
      stroke = Color.BLACK
      strokeWidth = 1
      centerX <== DVariable.this.x
      centerY <== DVariable.this.y

      fill <== when (hover) choose Color.LIGHTSALMON otherwise Color.LIGHTBLUE

      handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
        me.consume()
      }
    }

    private val nameText = new Text() {
      text = varName
      stroke = Color.BLACK
      x <== DVariable.this.x
      y <== DVariable.this.y
      mouseTransparent = true
    }

    x <== DConstraint.this.x + tx
    y <== DConstraint.this.y + ty

    content = Seq(circle, nameText)
  }

  // map from variable name to corresponding DVariable
  private var dVariableMap = Map[String, DVariable]()

  private val variables = new Group() {
    content = Seq()
  }

  // update appearance of the constraint
  def update() {
    val c = getConstraint()

    // avoid empty string being used to ensure the height is enough
    relText.text = if (c.relStr == "") " " else c.relStr

    var newDVariableMap = Map[String, DVariable]()
    variables.children = c.vars match {
      case None => Seq()
      case Some(vs) => vs map { varName =>
        dVariableMap.get(varName) match {
          case Some(dv) => {
            newDVariableMap += varName -> dv
            dv
          }
          case None => {
            val dv = new DVariable(varName, 20d * (dVariableMap.size + newDVariableMap.size), 20d)
            newDVariableMap += varName -> dv
            dv
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

  override val group = new Group(rectangle, relText, variables)
}
