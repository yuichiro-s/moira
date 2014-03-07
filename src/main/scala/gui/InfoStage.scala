package moira.gui

import scalafx.beans.property.ObjectProperty
import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane

import moira.gui.diagram.{Diagram,DObject,DParameter}
import moira.world.{ProtoParameter,ProtoConstraint}

class InfoStage(diagram: Diagram) extends Stage {
  title = "Info"
  scene = createEmptyInfoScene()

  width = 400
  height = 300

  // create a new /Scene/ if /diagram.infoObject/ changes.
  diagram.infoObject onChange { (_, _, odo) => 
    scene = odo match {
      case None => createEmptyInfoScene()
      case Some(obj) => {
        obj match {
          case dp: DParameter => createParameterInfoScene(dp)
          case _ => createEmptyInfoScene()
        }
      }
    }
  }

  private def createEmptyInfoScene(): Scene = {
    new Scene() {
      root = new BorderPane() {
        center = new Label("Empty")
      }
    }
  }

  private def createParameterInfoScene(dp: DParameter): Scene = {
    val pp: ObjectProperty[ProtoParameter] = dp.protoParameterProperty
    new Scene() {
      root = new BorderPane() {
        center = new Label("DParameter: " + pp().name)
      }
    }
  }
}
