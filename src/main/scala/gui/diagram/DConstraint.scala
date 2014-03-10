package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty,ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(pc0: ProtoConstraint, x0: Double, y0: Double)(implicit diagram: Diagram) extends DObject(diagram.selectedConstraints) {

  val cId = pc0.id

  // properties
  val x = DoubleProperty(x0)
  val y = DoubleProperty(y0)

  private val constraint = ObjectProperty(pc0)
  def getConstraint() = constraint()
  val constraintProperty = constraint

  val variableGroup = new Group()

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

  private val dVariables = ObjectProperty(Set[DVariable]())

  // update appearance of the constraint
  def update() {
    val c = constraint()

    // avoid empty string being used to ensure the height is enough
    relText.text = if (c.relStr == "") " " else c.relStr

    // update variables
    dVariables() = c.vars match {
      case None => Set[DVariable]()   // constraint is not well-defined yet
      case Some(vs) => vs.zipWithIndex map {
        case (varName, i) => {
          val dv = dVariables().find(_.varName == varName) match {
            case Some(dv) => dv
            case None => new DVariable(this, varName, 20d * i, 20d)
          }

          // update binding of the variable
          c.paramMap.get(varName) match {
            case None => {
              // the variable is not bound
              dv.dBinding() = None
            }
            case Some(pp) => {
              // the variable is bound to parameter /pp/
              val dp = diagram.dParameters().find(_.pId == pp.id)
              dp match {
                case None => throw new IllegalStateException(
                  "/DParameter/ with id=%d is not found" +
                    "in diagram.dParameters()".format(pp.id))
                case Some(dp) => {
                  dv.dBinding() = Some(new DBinding(dv, dp))
                }
              }
            }
          }

          dv
        }
      }
    }
  }

  // Update appearance whenever the constraint is changed.
  constraint onChange { update() }

  // synchronize constraint with /diagram.world()/
  diagram.world onChange {
    val pc = diagram.world().getConstraintById(cId)
    constraint() = pc match {
      case Some(pc) => pc
      case None => throw new IllegalStateException(
        "%s is not found in %s.".format(constraint(), diagram.world()))
    }
  }

  // update GUI
  dVariables onChange {
    variableGroup.content = dVariables().map(_.group)
  }

  override val group = new Group(rectangle, relText, variableGroup)

  // initialization
  update()
}
