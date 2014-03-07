package moira.gui

import moira.unit.PhysicalQuantity
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.stage.Stage
import scalafx.scene.{Node,Scene}
import scalafx.scene.control.{Label,TextField,ComboBox}
import scalafx.scene.layout.{Pane,BorderPane,HBox,VBox}

import moira.gui.diagram.{Diagram,DObject,DParameter,DConstraint}
import moira.world.{ProtoParameter,ProtoConstraint}
import moira.unit.{CommonDims,CommonUnits}

class InfoStage()(implicit diagram: Diagram) extends Stage {
  title = "Info"

  x = 900
  y = 100
  minWidth = 400
  minHeight = 300

  private val emptyInfoPane = new BorderPane() {
    center = new Label("Empty")
  }

  private val parameterInfoPane = new VBox() {
    val pp = ObjectProperty(ProtoParameter())

    val nameField = new TextField()
    val dimBox = new ComboBox[String]() {
      items = ObservableBuffer(CommonDims.nameToDim.keys.toSeq)
    }
    val unitBox = new ComboBox[String]()
    val valueField = new TextField()
    val lowerField = new TextField()
    val upperField = new TextField()
    val valueUnit = new Label()
    val lowerUnit = new Label()
    val upperUnit = new Label()

    // update GUI
    private def update() {
      val p = pp()

      nameField.text = p.name

      dimBox.value = CommonDims.dimToName.get(p.dim) match {
        case Some(name) => name
        case None => {
          // TODO: support dimensions not listed
          throw new UnsupportedOperationException
        }
      }

      unitBox.value = p.displayUnit

      def opqToStr(opq: Option[PhysicalQuantity]) = opq match {
        case Some(pq) => pq.value.toString
        case None => ""
      }
      valueField.text = opqToStr(p.value)
      lowerField.text = opqToStr(p.lower)
      upperField.text = opqToStr(p.upper)

      valueUnit.text = p.displayUnit
      lowerUnit.text = p.displayUnit
      upperUnit.text = p.displayUnit
    }

    pp.onChange(update)

    content = Seq(
      new Label("Parameter"),
      createItem("Name", nameField),
      createItem("Dim", dimBox),
      createItem("Unit", unitBox),
      createItem("Value", valueField, valueUnit),
      createItem("lower", lowerField, lowerUnit),
      createItem("upper", upperField, upperUnit)
    )
  }

  private val constraintInfoPane = new VBox() {

  }

  scene = new Scene() {
    root = emptyInfoPane

    // When an object is clicked, the scene switches to show the information.
    diagram.infoObject onChange { (_, _, odo) =>
      root = odo match {
        case None => emptyInfoPane
        case Some(obj) =>  obj match {
          case dp: DParameter => {
            parameterInfoPane.pp <== dp.parameterProperty
            parameterInfoPane
          }
          case dc: DConstraint => {
            //constraintInfoPane.pc <== dc.constraintProperty
            constraintInfoPane
          }
          case _ => emptyInfoPane
        }
      }
    }
  }

  private def createItem(name: String, item: Node*): HBox = {
    new HBox() {
      content = Seq(new Label(name)) ++ item
    }
  }

}
