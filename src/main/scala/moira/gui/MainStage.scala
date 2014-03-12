package moira.gui

import scalafx.application.JFXApp
import scalafx.scene.Scene

import moira.gui.diagram.Diagram
import scalafx.Includes._
import scalafx.scene.input.{KeyCode, KeyEvent}

object MainStage extends JFXApp.PrimaryStage {
  title = "Moira"
  scene = new Diagram()
}
