package moira.gui

import scalafx.Includes._
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.control.{Button,Label}
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

  private val updateButton = new Button("Update") {
    onAction = handle {
      diagram.infoObject() match {
        case Some(obj) => {
          obj match {
            case dp: DParameter => parameterInfoPane.updateParameter()
            case dc: DConstraint => constraintInfoPane.updateConstraint()
            case _ =>
          }
        }
        case _ =>
      }
    }
  }

  scene = new Scene() {
    root = new BorderPane() {
      center = emptyInfoPane
      bottom = updateButton

      // When an object is clicked, the scene switches to show the information.
      diagram.infoObject onChange { (_, _, odo) =>
        center = odo match {
          case None => emptyInfoPane
          case Some(obj) => obj match {
            case dp: DParameter => {
              parameterInfoPane.pId() = dp.id
              parameterInfoPane.updateControls()
              parameterInfoPane
            }
            case dc: DConstraint => {
              constraintInfoPane.cId() = dc.id
              constraintInfoPane.updateControls()
              constraintInfoPane
            }
            case _ => emptyInfoPane
          }
        }
      }
    }
  }
}
