package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty,ObjectProperty}
import scalafx.scene.Group
import scalafx.scene.layout.BorderPane
import scalafx.scene.input.{KeyCode,KeyCombination,KeyCodeCombination,MouseEvent}
import scalafx.scene.control.{MenuItem,MenuBar,Menu}

import moira.world.{ProtoParameter,World}
import moira.gui.InfoStage
import moira.unit.{PhysicalQuantity,SIDim,CommonDims}

class Diagram extends BorderPane {
  implicit val diagram: Diagram = this

  // properties
  val world = ObjectProperty(new World(Set.empty, Set.empty))
  val selectedParameters = ObjectProperty(Set[DParameter]())
  val infoObject: ObjectProperty[Option[DObject]] = ObjectProperty(None)
  // position the user pressed mouse button on the screen the last time
  val lastMousePressedX = DoubleProperty(200d)
  val lastMousePressedY = DoubleProperty(200d)

  //var connections = Seq[DConnection]()

  // parameters
  val pp1 = world().createParameter("abc", CommonDims.LENGTH, "km", None, None, None) match {
    case (w, pp) => { world() = w; pp }
  }
  val pp2 = world().createParameter("def", CommonDims.MASS, "kg", None, None, None) match {
    case (w, pp) => { world() = w; pp }
  }

  var dParameters = new Group() {
    children = Seq(
      new DParameter(pp1, 100, 100),
      new DParameter(pp2, 200, 150)
    )
  }

  // constraints
  val pc1 = world().createConstraint("$x+$y=1km", Map()) match {
    case (w, pc) => { world() = w; pc }
  }
  var dConstraints = new Group() {
    children = Seq(
      new DConstraint(pc1, 100, 200)
    )
  }

  // operations

  def createConstraint(relStr: String = "", paramMap: Map[String, ProtoParameter] = Map()) {
    val x = lastMousePressedX()
    val y = lastMousePressedY()

    world() = world().createConstraint(relStr, paramMap) match {
      case (w, pc) => {
        val newDConstraint = new DConstraint(pc, x, y)
        dConstraints.children += newDConstraint
        w
      }
    }
  }

  def createParameter(
    name: String = "",
    dim: SIDim = SIDim(),
    displayUnit: String = "",
    lower: Option[PhysicalQuantity] = None,
    upper: Option[PhysicalQuantity] = None,
    value: Option[PhysicalQuantity] = None
  ) {
    val x = lastMousePressedX()
    val y = lastMousePressedY()

    world() = world().createParameter(
      name, dim, displayUnit, lower, upper, value) match {
      case (w, pp) => {
        val newDParameter = new DParameter(pp, x, y)
        dParameters.children += newDParameter
        w
      }
    }
  }

  // menu bar
  val menuBar = new MenuBar {
    menus = Seq(
      new Menu("Operations") {
        items = Seq(
          new MenuItem("New Parameter") {
            accelerator = new KeyCodeCombination(KeyCode.P,
              KeyCombination.ShortcutDown)
            onAction = handle { createParameter() }
          },
          new MenuItem("New Constraint") {
            accelerator = new KeyCodeCombination(KeyCode.R,
              KeyCombination.ShortcutDown)
            onAction = handle { createConstraint() }
          },
          new MenuItem("Bind Variable") {
            accelerator = new KeyCodeCombination(KeyCode.B,
              KeyCombination.ShortcutDown)
            onAction = handle {
              println("Bind Variable selected.")
            }
          }
        )
      }
    )
  }

  handleEvent(MouseEvent.MousePressed) { me: MouseEvent =>
    // When empty space is clicked, the info window becomes empty.
    infoObject() = None

    // change last mouse pressed position
    lastMousePressedX() = me.x
    lastMousePressedY() = me.y
  }

  top = menuBar
  center = new Group(dParameters, dConstraints)

  val infoStage = new InfoStage()
  infoStage.show()
}

