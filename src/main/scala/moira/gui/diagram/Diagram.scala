package moira.gui.diagram

import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.{Scene, Group}
import scalafx.scene.layout.BorderPane
import scalafx.scene.input.{KeyCode,KeyEvent,KeyCombination,KeyCodeCombination,MouseEvent}
import scalafx.scene.control.{MenuItem,MenuBar,Menu}
import scalafx.stage.{FileChooser,Stage}

import java.io.File
import scala.xml.XML

import moira.world.{ProtoParameter,ProtoConstraint,World}
import moira.gui.InfoStage
import moira.unit.{PhysicalQuantity,SIDim}
import moira.constraint.solver.ConstraintSolver
import moira.constraint.Constraint

class Diagram extends Scene(400, 300) {
  implicit val diagram: Diagram = this

  // properties
  val world = ObjectProperty(new World(Set.empty, Set.empty))

  val dParameters = ObjectProperty(Map[Int, DParameter]())
  val dConstraints = ObjectProperty(Map[Int, DConstraint]())
  val dVariables = ObjectProperty(Map[(Int, String), DVariable]())
  val dBindings = ObjectProperty(Map[((Int, String), Int), DBinding]())

  val selectedParameters = ObjectProperty(Set[Int]())
  val selectedConstraints = ObjectProperty(Set[Int]())
  val selectedVariables = ObjectProperty(Set[(Int, String)]())
  val selectedBindings = ObjectProperty(Set[((Int, String), Int)]())

  val infoObject: ObjectProperty[Option[DObject[Int]]] = ObjectProperty(None)

  case class MouseInfo(lastPressedX: Double, lastPressedY: Double)
  val mouseInfo = ObjectProperty(MouseInfo(200d, 200d))

  val filePath: ObjectProperty[Option[String]] = ObjectProperty(None)

  // Unselects objects.
  def unselect() {
    selectedParameters() = Set.empty
    selectedConstraints() = Set.empty
    selectedVariables() = Set.empty
    selectedBindings() = Set.empty
  }

  val parameterGroup = new Group()
  val constraintGroup = new Group()
  val variableGroup = new Group()
  val bindingGroup = new Group()

  // operations

  def createConstraint(relStr: String = "", paramMap: Map[String, Int] = Map()) {
    val x = mouseInfo().lastPressedX
    val y = mouseInfo().lastPressedY

    world() = world().createConstraint(relStr, paramMap) match {
      case (w, pc) => {
        val newDc = new DConstraint(pc.id, x, y)(diagram)
        infoObject() = Some(newDc)
        dConstraints() += pc.id -> newDc
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
        val newDp = new DParameter(pp.id, x, y)(diagram)
        infoObject() = Some(newDp)
        dParameters() += pp.id -> newDp
        w
      }
    }
  }

  def createBinding(pId: Int, vIds: Set[(Int, String)]) {
    vIds foreach {
      case (cId, varName) => {
        val (newWorld, _) = world().addConnection(cId, varName, pId)

        // update world
        world() = newWorld
      }
    }
  }

  def removeSelectedObjects() {
    var tmpWorld = world()

    // remove bindings
    selectedBindings() foreach {
      case ((cId, varName), _) => {
        // remove binding
        tmpWorld = tmpWorld.removeConnection(cId, varName)._1
      }
    }

    // remove constraints
    selectedConstraints() foreach { cId =>
      tmpWorld = tmpWorld.removeConstraint(cId)
    }

    // remove parameters and connected bindings
    selectedParameters() foreach { pId =>
      // remove bindings connected to the parameter
      tmpWorld.constraints foreach { pc =>
        pc.paramMap foreach {
          case (varName, pId2) => {
            if (pId == pId2) {
              tmpWorld = tmpWorld.removeConnection(pc.id, varName)._1
            }
          }
        }
      }

      // remove parameter
      tmpWorld = tmpWorld.removeParameter(pId)
    }

    // update /world/
    world() = tmpWorld
  }

  // Calculates parameter values which satisfy all selected constraints,
  // and binds the values to the parameters
  def calculate() {
    // selected constraints
    val pcs: Set[ProtoConstraint] = world().constraints.filter { pc =>
      selectedConstraints().contains(pc.id)
    }

    try {
      val pMap: Map[Int, ProtoParameter] = (world().parameters map { pp =>
        pp.id -> pp
      }).toMap
      val cs: Set[Constraint] = pcs.map(_.toConstraint(pMap))

      ConstraintSolver.solve(cs) match {
        case None => {
          println("Failed to solve the constraints.")
        }
        case Some(bs) => {
          var tmpWorld = world()
          bs foreach {
            case (p, pq) => {
              tmpWorld.getParameterById(p.id) match {
                case None => {
                  throw new IllegalStateException(
                    "Parameter(id=%d) is not found in %s.".format(p.id, world()))
                }
                case Some(pp) => {
                  // bind calculated value to the parameter
                  tmpWorld = tmpWorld.updateParameter(
                    pp.id, pp.name, pp.dim, pp.displayUnit, pp.lower, pp.upper, Some(pq))._1
                }
              }
            }
          }
          world() = tmpWorld
        }
      }
    } catch {
      case e: Exception => println(e)
    }
  }

  def selectAll() {
    selectedConstraints() = dConstraints().keys.toSet
    selectedVariables() = dVariables().keys.toSet
    selectedParameters() = dParameters().keys.toSet
    selectedBindings() = dBindings().keys.toSet
  }

  def unsetVariables() {
    var newWorld = world()

    selectedParameters() foreach { pId =>
      val pp: ProtoParameter = world().getParameterById(pId) match {
        case Some(pp) => pp
        case None =>
          throw new IllegalStateException(
            s"Parameter(id=$pId) is not found in ${world()}.")
      }
      newWorld = newWorld.updateParameter(pp.id, pp.name, pp.dim, pp.displayUnit, pp.lower, pp.upper, None)._1
    }

    world() = newWorld
  }

  def save() {
    val fileChooser = new FileChooser()
    val f: File = fileChooser.showSaveDialog(new Stage())
    if (f != null) {
      XML.save(f.getPath(), toXML(), "UTF-8")
    }
  }

  def open() {
    val fileChooser = new FileChooser()
    val f: File = fileChooser.showOpenDialog(new Stage())
    if (f != null) {
      val xml = XML.loadFile(f)
      loadXML(xml)
    }
  }

  // menu bar
  val menuBar = new MenuBar {
    menus = Seq(

      new Menu("File") {
        items = Seq(
          new MenuItem("Save") {
            accelerator = new KeyCodeCombination(KeyCode.S,
              KeyCombination.ShortcutDown)
            onAction = handle { save() }
          },
          new MenuItem("Open") {
            accelerator = new KeyCodeCombination(KeyCode.O,
              KeyCombination.ShortcutDown)
            onAction = handle { open() }
          }
        )
      },

      new Menu("Diagram") {
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
                // if exactly one parameter and more than 0 variables are selected
                createBinding(selectedParameters().head, selectedVariables())
              } else {
                println("Parameter and variables have to be selected.")
              }
            }
          },
          new MenuItem("Unset Variables") {
            accelerator = new KeyCodeCombination(KeyCode.D,
              KeyCombination.ShortcutDown)
            onAction = handle { unsetVariables() }
          },
          new MenuItem("Select All") {
            accelerator = new KeyCodeCombination(KeyCode.A,
              KeyCombination.ShortcutDown)
            onAction = handle { selectAll() }
          },
          new MenuItem("Calculate") {
            accelerator = new KeyCodeCombination(KeyCode.K,
              KeyCombination.ShortcutDown)
            onAction = handle { calculate() }
          }
        )
      }
    )
  }

  def startDrag(x: Double, y: Double) {
    mouseInfo() = MouseInfo(x, y)

    val dvs: Set[DVariable] = selectedVariables().flatMap { id => dVariables().get(id) }
    val dcs: Set[DConstraint] = selectedConstraints().flatMap { id => dConstraints().get(id) }
    val dps: Set[DParameter] = selectedParameters().flatMap { id => dParameters().get(id) }

    dvs.foreach { dv => dv.setDragInfo(dv.tx(), dv.ty()) }
    dcs.foreach { dc => dc.setDragInfo(dc.x(), dc.y()) }
    dps.foreach { dp => dp.setDragInfo(dp.x(), dp.y()) }
  }

  def drag(x: Double, y: Double) {
    val dvs: Set[DVariable] = selectedVariables().flatMap { id => dVariables().get(id) }
    val dcs: Set[DConstraint] = selectedConstraints().flatMap { id => dConstraints().get(id) }
    val dps: Set[DParameter] = selectedParameters().flatMap { id => dParameters().get(id) }

    val dx = x - mouseInfo().lastPressedX
    val dy = y - mouseInfo().lastPressedY

    dvs foreach { dv =>
      if (!selectedConstraints().contains(dv.cId)) {
        // /DVariable/ follows its parent /DConstraint/, so don't move /dv/
        // when its parent /dc/ is also dragged.
        dv.tx() = dv.dragInfo().x0 + dx
        dv.ty() = dv.dragInfo().y0 + dy
      }
    }
    dcs foreach { dc =>
      dc.x() = dc.dragInfo().x0 + dx
      dc.y() = dc.dragInfo().y0 + dy
    }
    dps foreach { dp =>
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
    center = new Group(bindingGroup, variableGroup, constraintGroup, parameterGroup) {
      managed = false
    }
    top = menuBar
  }

  val infoStage = new InfoStage()
  infoStage.show()

  // update GUI
  dParameters onChange {
    parameterGroup.content = dParameters().values.map(_.group)
  }
  dConstraints onChange {
    constraintGroup.content = dConstraints().values.map(_.group)
  }
  dVariables onChange {
    variableGroup.content = dVariables().values.map(_.group)
  }
  dBindings onChange {
    bindingGroup.content = dBindings().values.map(_.group)
  }

  // synchronize /dParameters/, /dConstraints/, /dVariables/ and /dBindings/ with /world/
  world onChange {
    val pps = world().parameters
    val pcs = world().constraints

    // Remove objects that do not exist in /world/ any more.
    dParameters() = dParameters().filter { case (id, _) => pps.exists(_.id == id) }
    dConstraints() = dConstraints().filter { case (id, _) => pcs.exists(_.id == id) }
    dVariables() = dVariables().filter {
      case ((cId, varName), _) => {
        pcs.find(_.id == cId) match {
          case None => false  // parent constraint does not exist
          case Some(pc) => {
            pc.vars match {
              case None => false  // parent constraint is not parseable
              case Some(vs) => vs.contains(varName)
            }
          }
        }
      }
    }
    dBindings() = dBindings().filter {
      case (((cId, varName), pId), _) => {
        (pcs.find(_.id == cId), pps.find(_.id == pId)) match {
          case (Some(pc), Some(pp)) => pc.paramMap.get(varName) match {
            case None => false
            case Some(id) => id == pp.id  // connected to the right parameter
          }
          case _ => false // either constraint or parameter doesn't exist
        }
      }
    }

    // Create /DVariable/s and /DBinding/s
    pcs foreach { pc =>
      val cId = pc.id
      pc.vars match {
        case None =>
        case Some(vs) => {
          vs.zipWithIndex foreach {
            case (varName, i) => {
              val vId = (cId, varName)
              if (!dVariables().isDefinedAt(vId)) {
                // If /DVariable/ has not been created, create one.
                val newDv = new DVariable(cId, varName, i * 40d, 40d)(diagram)
                dVariables() += vId -> newDv
              }

              pc.paramMap.get(varName) match {
                case None =>
                case Some(pId) => {
                  val bId = (vId, pId)
                  if (!dBindings().isDefinedAt(bId)) {
                    // If /DBinding/ has not been created, create one.
                    val newDb = new DBinding(vId, pId)(diagram)
                    dBindings() += bId -> newDb
                  }
                }
              }
            }
          }
        }
      }
    }

    // If the object which the info window is currently showing no longer exists,
    // set the info window to empty.
    infoObject() match {
      case Some(dp: DParameter) => {
        if (!pps.exists(_.id == dp.id)) {
          infoObject() = None
        }
      }
      case Some(dc: DConstraint) => {
        if (!pcs.exists(_.id == dc.id)) {
          infoObject() = None
        }
      }
      case _ =>
    }
  }

  def toXML(): xml.Elem = {
    <world>
      { dConstraints().values.map(_.toXML()) }
      { dParameters().values.map(_.toXML()) }
    </world>
  }

  def loadXML(source: xml.Elem) {
    def getPos(node: xml.Node): (Double, Double) = {
      val x: Double = (node \ "x").text.toDouble
      val y: Double = (node \ "y").text.toDouble
      (x, y)
    }

    try {
      // create parameters
      val (pps, dps) = (source \ "parameter" map { node =>
        val pp = ProtoParameter.fromXML(node)
        val (x, y) = getPos(node)
        (pp, new DParameter(pp.id, x, y)(diagram))
      }).unzip

      dParameters() = (dps.toSet map { dp: DParameter =>
        dp.id -> dp
      }).toMap

      // create constraints
      val (pcs, dcs, dvbss) = (source \ "constraint" map { node =>
        val pc = ProtoConstraint.fromXML(node)
        val cId = pc.id

        val (x, y) = getPos(node)
        val dc = new DConstraint(cId, x, y)(diagram)

        val dvbs: (Set[DVariable], Set[Option[DBinding]]) = pc.vars match {
          case None => (Set.empty, Set.empty)
          case Some(vs) => {
            (vs map { varName =>
              val (x, y) = ((node \\ "var") collectFirst {
                case n if (n \\ "name").text == varName => {
                  ((n \\ "x").text.toDouble, (n \\ "y").text.toDouble)
                }
              }).get
              val dv = new DVariable(cId, varName, x, y)(diagram)
              val db = pc.paramMap.get(varName).map { pId =>
                new DBinding((cId, varName), pId)(diagram)
              }

              (dv, db)
            }).unzip
          }
        }

        (pc, dc, dvbs)
      }).unzip3

      val (dvss, dbss) = dvbss.unzip
      val dvs: Seq[DVariable] = dvss.flatten
      val dbs: Seq[DBinding] = dbss.flatten collect { case Some(db) => db }

      dConstraints() = (dcs.toSet map { dc: DConstraint =>
        dc.id -> dc
      }).toMap

      dVariables() = (dvs.toSet map { dv: DVariable =>
        dv.id -> dv
      }).toMap

      dBindings() = (dbs.toSet map { db: DBinding =>
        db.id -> db
      }).toMap

      val nextParameterId = dps.maxBy(_.id).id + 1
      val nextConstraintId = dcs.maxBy(_.id).id + 1

      world() = new World(pcs.toSet, pps.toSet, nextConstraintId, nextParameterId)

    } catch {
      case e: NumberFormatException => println(e)
      case e: NoSuchElementException => println(e)
    }
  }
}
