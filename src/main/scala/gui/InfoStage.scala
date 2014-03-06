package moira.gui

import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane

import moira.gui.diagram.Diagram

class InfoStage extends Stage {
  title = "Info"
  scene = new Scene(400, 300) {
    root = new BorderPane()
  }
}
