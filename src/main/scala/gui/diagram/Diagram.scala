package moira.gui.diagram

import scalafx.scene.Group
import scalafx.scene.layout.Pane

import moira.world.World

class Diagram extends Pane {

  implicit val diagram: Diagram = this

  var world = new World(Set.empty, Set.empty)

  //var connections = Seq[DConnection]()
  //var constraints = Seq[DConstraint]()
  var dParams = new Group() {
    children = Seq(
      new DParameter(1, 100, 100),
      new DParameter(2, 150, 200)
    )
  }

  content = Seq(dParams)
}

