package moira.gui

import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane

import moira.gui.diagram.{Diagram,DParameter,DConstraint}

class InfoStage()(implicit diagram: Diagram) extends Stage {
  title = "Info"

  x = 900
  y = 100
  minWidth = 400
  minHeight = 300

  private val emptyInfoPane = new BorderPane() {
    center = new Label("Empty")
  }

  // information of parameter
  private val parameterInfoPane = new ParameterInfoPane()

  // information of constraint
  private val constraintInfoPane = new ConstraintInfoPane()

  scene = new Scene() {
    root = new BorderPane() {
      center = emptyInfoPane

      // When an object is clicked, the scene switches to show the information.
      diagram.infoObject onChange { (_, _, odo) =>
        center = odo match {
          case None => emptyInfoPane
          case Some(obj) => obj match {
            case dp: DParameter => {
              parameterInfoPane.pId() = Some(dp.id)
              constraintInfoPane.cId() = None
              parameterInfoPane
            }
            case dc: DConstraint => {
              parameterInfoPane.pId() = None
              constraintInfoPane.cId() = Some(dc.id)
              constraintInfoPane
            }
            case _ => emptyInfoPane
          }
        }
      }
    }
  }
}
