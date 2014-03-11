package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.{Scene, Group}
import scalafx.scene.layout.BorderPane
import scalafx.scene.input.{KeyCode,KeyEvent,KeyCombination,KeyCodeCombination,MouseEvent}
import scalafx.scene.control.{MenuItem,MenuBar,Menu}

import moira.world.{ProtoParameter,ProtoConstraint,World}
import moira.gui.InfoStage
import moira.unit.{PhysicalQuantity,SIDim,CommonDims}

class Diagram extends Scene(400, 300) {
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

  case class MouseInfo(lastPressedX: Double, lastPressedY: Double)
  val mouseInfo = ObjectProperty(MouseInfo(200d, 200d))

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
    val x = mouseInfo().lastPressedX
    val y = mouseInfo().lastPressedY

    world() = world().createConstraint(relStr, paramMap) match {
      case (w, pc) => {
        val newPc = new DConstraint(pc, x, y)
        infoObject() = Some(newPc)
        dConstraints() += newPc
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
    val x = mouseInfo().lastPressedX
    val y = mouseInfo().lastPressedY

    world() = world().createParameter(
      name, dim, displayUnit, lower, upper, value) match {
      case (w, pp) => {
        val newDp = new DParameter(pp, x, y)
        infoObject() = Some(newDp)
        dParameters() += newDp
        w
      }
    }
  }

  def createBinding(dp: DParameter, dvs: Set[DVariable]) {
    dvs foreach { dv: DVariable =>
      val dc: DConstraint = dv.constraint
      val pc: ProtoConstraint = dc.getConstraint()

      val (newWorld, _) = world().updateConstraint(pc.id, pc.relStr, pc.paramMap.updated(dv.varName, dp.getParameter()))

      // update world
      world() = newWorld
    }
  }

  def removeSelectedObjects() {
    val dbs: Set[DBinding] = selectedBindings() collect { case b: DBinding => b }
    val dcs: Set[DConstraint] = selectedConstraints() collect { case c: DConstraint => c }
    val dps: Set[DParameter] = selectedParameters() collect { case p: DParameter => p }

    var tmpWorld = world()

    // remove bindings
    dbs foreach { b =>
      val dv = b.variable
      val dp = b.parameter
      val dc = dv.constraint
      val pc = dc.getConstraint()

      assert(dp.getParameter() == pc.paramMap(dv.varName),
        "Variable %s is not bound to %s.".format(dv.varName, dp.getParameter()))

      // remove binding
      tmpWorld = tmpWorld.updateConstraint(pc.id, pc.relStr, pc.paramMap - dv.varName)._1
    }

    // remove constraints
    dcs foreach { dc =>
      tmpWorld = tmpWorld.removeConstraint(dc.cId)
    }

    // remove parameters and connected bindings
    dps foreach { dp =>
      // remove bindings connected to the parameter
      tmpWorld.constraints foreach { pc =>
        pc.paramMap foreach {
          case (varName, pp) => {
            if (pp.id == dp.pId) {
              assert(dp.getParameter() == pc.paramMap(varName),
                "The state of parameter(id=%d) is inconsistent.".format(pp.id))
              tmpWorld = tmpWorld.updateConstraint(pc.id, pc.relStr, pc.paramMap - varName)._1
            }
          }
        }
      }

      // remove parameter
      tmpWorld = tmpWorld.removeParameter(dp.pId)
    }

    // update /world/
    world() = tmpWorld
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

  def startDrag(x: Double, y: Double) {
    mouseInfo() = MouseInfo(x, y)

    val dvs: Set[DVariable] = selectedVariables() collect { case v: DVariable => v }
    val dcs: Set[DConstraint] = selectedConstraints() collect { case c: DConstraint => c }
    val dps: Set[DParameter] = selectedParameters() collect { case p: DParameter => p }

    dvs.foreach { dv => dv.setDragInfo(dv.tx(), dv.ty()) }
    dcs.foreach { dc => dc.setDragInfo(dc.x(), dc.y()) }
    dps.foreach { dp => dp.setDragInfo(dp.x(), dp.y()) }
  }

  def drag(x: Double, y: Double) {
    val dvs: Set[DVariable] = selectedVariables() collect { case v: DVariable => v }
    val dcs: Set[DConstraint] = selectedConstraints() collect { case c: DConstraint => c }
    val dps: Set[DParameter] = selectedParameters() collect { case p: DParameter => p }

    val dx = x - mouseInfo().lastPressedX
    val dy = y - mouseInfo().lastPressedY

    dvs.foreach { dv =>
      if (!dcs.contains(dv.constraint)) {
        // /DVariable/ follows its parent /DConstraint/, so don't move /dv/
        // when its parent /dc/ is also dragged.
        dv.tx() = dv.dragInfo().x0 + dx
        dv.ty() = dv.dragInfo().y0 + dy
      }
    }
    dcs.foreach { dc =>
      dc.x() = dc.dragInfo().x0 + dx
      dc.y() = dc.dragInfo().y0 + dy
    }
    dps.foreach { dp =>
      dp.x() = dp.dragInfo().x0 + dx
      dp.y() = dp.dragInfo().y0 + dy
    }
  }

  onMousePressed = { me: MouseEvent =>
    // When empty space is clicked, the info window becomes empty.
    infoObject() = None

    // change last mouse pressed position
    mouseInfo() = MouseInfo(me.x, me.y)

    // unselect objects
    unselect()
  }

  // key event
  onKeyPressed = { ke: KeyEvent =>
    ke.code match {
      case KeyCode.DELETE | KeyCode.BACK_SPACE => removeSelectedObjects()
      case _ =>
    }
  }

  root = new BorderPane() {
    top = menuBar
    center = new Group(constraintGroup, parameterGroup)
  }

  val infoStage = new InfoStage()
  infoStage.show()

  // update GUI
  dParameters onChange {
    parameterGroup.content = dParameters().map(_.group)
  }
  dConstraints onChange {
    constraintGroup.content = dConstraints().map(_.group)
  }

  // synchronize /dParameters/ and /dConstraints/ with /world/
  world onChange {
    val pps = world().parameters
    val pcs = world().constraints

    // Remove parameters and constraints that are not in /world/ any more.
    dParameters() = dParameters().filter { dp => pps.exists(_.id == dp.pId) }
    dConstraints() = dConstraints().filter { dc => pcs.exists(_.id == dc.cId) }

    // If the object which the info window is currently showing no longer exists,
    // set the info window to empty.
    infoObject() match {
      case Some(dp: DParameter) => {
        if (!pps.exists(_.id == dp.pId)) {
          infoObject() = None
        }
      }
      case Some(dc: DConstraint) => {
        if (!pcs.exists(_.id == dc.cId)) {
          infoObject() = None
        }
      }
      case _ =>
    }
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
