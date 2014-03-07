package moira.gui

import scalafx.application.JFXApp
import scalafx.scene.Scene

import moira.gui.diagram.Diagram

object MainStage extends JFXApp.PrimaryStage {
  title = "Moira"
  scene = new Scene(400, 300) {
    val diagram = new Diagram()
    root = diagram
  }
}
