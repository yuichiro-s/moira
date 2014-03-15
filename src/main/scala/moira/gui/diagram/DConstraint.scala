package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.paint.Color

import moira.world.ProtoConstraint

class DConstraint(pc0: ProtoConstraint, x0: Double, y0: Double)(implicit val diagram: Diagram) extends DObject(diagram.selectedConstraints) with Draggable {

  val cId = pc0.id

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

  private val constraint = ObjectProperty(pc0)
  def getConstraint() = constraint()
  val constraintProperty = constraint

  val variableGroup = new Group()

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

  val dVariables = ObjectProperty(Set[DVariable]())

  // update appearance of the constraint
  def update() {
    val c = constraint()

    // avoid empty string being used to ensure the height is enough
    relText.text = if (c.relStr == "") " " else c.relStr

    isDefined() = c.rel.isDefined

    rectangle.strokeDashArray = if (isDefined()) null else UNDEFINED_SEQ

    // update variables
    dVariables() = c.vars match {
      case None => Set[DVariable]()   // constraint is not well-defined yet
      case Some(vs) => vs.zipWithIndex map {
        case (varName, i) => {
          val dv = dVariables().find(_.varName == varName) match {
            case Some(dv) => dv
            case None => new DVariable(this, varName, 40d * i, 40d)
          }

          dv.isBound() = c.paramMap.isDefinedAt(varName)

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
      case None => {
        // This parameter has been removed.
        constraint() // This return value is meaningless.
      }
    }
  }

  // update GUI
  dVariables onChange {
    variableGroup.content = dVariables().map(_.group)
  }

  override val group = new Group(variableGroup, rectangle, relText)

  def toXML(): xml.Elem = {
    val c = constraint()
    def vToNode(v: String) = {
      dVariables().find(_.varName == v) match {
        case Some(dv) => {
          <var>
            <name>{v}</name>
            {c.paramMap.get(v) match {
              case None =>
              case Some(pp) => <pid>{pp.id}</pid>
            }}
            <x>{dv.tx()}</x>
            <y>{dv.ty()}</y>
          </var>
        }
        case None => throw new NoSuchElementException(
          "Unknown variable %s in %s.".format(v, c))
      }
    }

    <constraint>
      <id>{c.id}</id>
      <rel>{c.relStr}</rel>
      {c.vars match {
        case None =>
        case Some(vs) => <vars>{ for (v <- vs) yield vToNode(v) }</vars>
      }}
      <x>{x()}</x>
      <y>{y()}</y>
    </constraint>
  }

  // initialization
  update()
}
