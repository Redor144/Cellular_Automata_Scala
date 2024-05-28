import javax.swing.{JFrame, WindowConstants}

object Program extends JFrame {
  private val serialVersionUID: Long = 1L
  private var gof: GUI = _

  def apply(): Unit = {
    setTitle("Pedestrian simulation")
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    gof = new GUI(this)
    gof.initialize(getContentPane)

    setSize(800, 600)
    setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    Program()
  }
}
