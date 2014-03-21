package moira.gui

import moira.gui.diagram.Diagram

import scalafx.scene.layout.VBox
import scalafx.scene.control.Label

class WorldInfoPane()(implicit diagram: Diagram) extends VBox {

  val parameterCountLabel = new Label()
  val constraintCountLabel = new Label()

  def updateControls() {
    val cs = diagram.world().constraints
    val ps = diagram.world().parameters

    parameterCountLabel.text = s"${ps.size} parameter(s)"
    constraintCountLabel.text = s"${cs.size} constraint(s)"
  }

  diagram.world onChange {
    updateControls()
  }

  content = Seq(
    parameterCountLabel,
    constraintCountLabel
  )

}
