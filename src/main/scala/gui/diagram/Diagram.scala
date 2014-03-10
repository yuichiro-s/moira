package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.scene.Group
import scalafx.scene.layout.BorderPane
import scalafx.scene.input.{KeyCode,KeyCombination,KeyCodeCombination,MouseEvent}
import scalafx.scene.control.{MenuItem,MenuBar,Menu}

import moira.world.{ProtoParameter,ProtoConstraint,World}
import moira.gui.InfoStage
import moira.unit.{PhysicalQuantity,SIDim,CommonDims}

class Diagram extends BorderPane {
  implicit val diagram: Diagram = this

  // properties
  val world = ObjectProperty(new World(Set.empty, Set.empty))
  val dParameters = ObjectProperty(Set[DParameter]())
  val dConstraints = ObjectProperty(Set[DConstraint]())

  // TODO: sets below have to be typed more specifically
  val selectedParameters = ObjectProperty(Set[DObject]())
  val selectedConstraints = ObjectProperty(Set[DObject]())
  val selectedVariables = ObjectProperty(Set[DObject]())
  val selectedBindings = ObjectProperty(Set[DObject]())

  val infoObject: ObjectProperty[Option[DObject]] = ObjectProperty(None)
  // position the user pressed mouse button on the screen the last time
  val lastMousePressedX = DoubleProperty(200d)
  val lastMousePressedY = DoubleProperty(200d)

  // Unselects objects.
  def unselect() {
    selectedParameters() = Set[DObject]()
    selectedConstraints() = Set[DObject]()
    selectedVariables() = Set[DObject]()
    selectedBindings() = Set[DObject]()
  }

  val parameterGroup = new Group()
  val constraintGroup = new Group()

  // operations

  def createConstraint(relStr: String = "", paramMap: Map[String, ProtoParameter] = Map()) {
    val x = lastMousePressedX()
    val y = lastMousePressedY()

    world() = world().createConstraint(relStr, paramMap) match {
      case (w, pc) => {
        dConstraints() += new DConstraint(pc, x, y)
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
        dParameters() +=  new DParameter(pp, x, y)
        w
      }
    }
  }

  def createBinding(dp: DParameter, dvs: Set[DVariable]) {
    dvs foreach { dv: DVariable =>
      val dc: DConstraint = dv.constraint
      val pc: ProtoConstraint = dc.getConstraint()

      val (newWorld, c) = world().updateConstraint(pc.id, pc.relStr, pc.paramMap.updated(dv.varName, dp.getParameter()))

      println(c)

      // update world
      world() = newWorld
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
              if (selectedParameters().size == 1 &&
                  selectedVariables().size > 0 &&
                  selectedConstraints().size == 0 &&
                  selectedBindings().size == 0) {
                // if excatly one parameter and more than 0 variables are selected
                val dp: DParameter = selectedParameters().head match {
                  case dp: DParameter => dp
                  case _ => throw new IllegalStateException("Non-parameter found in /selectedParameters/.")
                }
                val dvs: Set[DVariable] = selectedVariables() map {
                  case dv: DVariable => dv
                  case _ => throw new IllegalStateException("Non-variable found in /selectedVariables/.")
                }
                createBinding(dp, dvs)
              } else {
                println("Parameter and variables have to be selected.")
              }
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

    // unselect objects
    unselect()
  }

  val infoStage = new InfoStage()
  infoStage.show()

  top = menuBar
  center = new Group(constraintGroup, parameterGroup)

  // update GUI
  dParameters onChange {
    parameterGroup.content = dParameters().map(_.group)
  }
  dConstraints onChange {
    constraintGroup.content = dConstraints().map(_.group)
  }

  // initial parameters
  val pp1 = world().createParameter("abc", CommonDims.LENGTH, "km", None, None, None) match {
    case (w, pp) => { world() = w; pp }
  }
  val pp2 = world().createParameter("def", CommonDims.MASS, "kg", None, None, None) match {
    case (w, pp) => { world() = w; pp }
  }
  dParameters() = Set(
    new DParameter(pp1, 100, 100),
    new DParameter(pp2, 200, 150)
  )

  // initial constraints
  val pc1 = world().createConstraint("$x+$y=1km", Map()) match {
    case (w, pc) => { world() = w; pc }
  }
  dConstraints() = Set(
    new DConstraint(pc1, 100d, 200d)
  )
}
