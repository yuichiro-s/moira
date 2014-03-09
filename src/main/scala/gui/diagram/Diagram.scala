package moira.gui.diagram

import scalafx.beans.property.ObjectProperty
import scalafx.scene.Group
import scalafx.scene.layout.Pane
import scalafx.scene.input.MouseEvent

import moira.world.World
import moira.gui.InfoStage
import moira.unit.CommonDims

class Diagram extends Pane {
  implicit val diagram: Diagram = this

  // properties
  val world = ObjectProperty(new World(Set.empty, Set.empty))
  val selectedParameters = ObjectProperty(Set[DParameter]())
  val infoObject: ObjectProperty[Option[DObject]] = ObjectProperty(None)

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

  // When empty space is clicked, the info window becomes empty.
  handleEvent(MouseEvent.MousePressed) { infoObject() = None }

  content = Seq(dParameters, dConstraints)

  val infoStage = new InfoStage()
  infoStage.show()
}

