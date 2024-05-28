import java.awt.event.ActionListener
import java.awt.{BorderLayout, Container, Dimension, event}
import javax.swing.{JButton, JFrame, JPanel, JSlider, Timer}
import javax.swing.event.{ChangeEvent, ChangeListener}
//
class GUI(jf: JFrame) extends JPanel with ActionListener with ChangeListener {
  private val serialVersionUID: Long = 1L
  private val timer: Timer = new Timer(initDelay, this)
  timer.stop()
  private var board: Board = _
  private val start: JButton = new JButton("Start")
  private val clear: JButton = new JButton("Clear")
  private val pred: JSlider = new JSlider
  private val frame: JFrame = jf
  private var iterNum: Int = 0
  private val maxDelay: Int = 500
  private val initDelay: Int = 100
  private var running: Boolean = false

  def initialize(container: Container): Unit = {
    container.setLayout(new BorderLayout)
    container.setSize(new Dimension(1024, 768))
    val buttonPanel: JPanel = new JPanel

    start.setActionCommand("Start")
    start.setToolTipText("Starts clock")
    start.addActionListener(this)

    clear.setActionCommand("clear")
    clear.setToolTipText("Clears the board")
    clear.addActionListener(this)

    pred.setMinimum(0)
    pred.setMaximum(maxDelay)
    pred.setToolTipText("Time speed")
    pred.addChangeListener(this)
    pred.setValue(maxDelay - timer.getDelay)

    buttonPanel.add(start)
    buttonPanel.add(clear)
    buttonPanel.add(pred)

    board = new Board(1024, 768 - buttonPanel.getHeight)
    container.add(board, BorderLayout.CENTER)
    container.add(buttonPanel, BorderLayout.SOUTH)
  }

  override def actionPerformed(e: event.ActionEvent): Unit = {
    if (e.getSource.equals(timer)) {
      iterNum += 1
      frame.setTitle("Game of Life (" + iterNum.toString + " iteration)")
      board.iteration()
    } else {
      val command: String = e.getActionCommand
      if (command.equals("Start")) {
        if (!running) {
          timer.start()
          start.setText("Pause")
        } else {
          timer.stop()
          start.setText("Start")
        }
        running = !running
        clear.setEnabled(true)
      }
      else if (command.equals("clear")) {
        iterNum = 0
        timer.stop()
        start.setEnabled(true)
        board.clear()
        frame.setTitle("Cellurlar Autmata Toolbox")
      }
    }
  }

  override def stateChanged(e: ChangeEvent): Unit = {
    timer.setDelay(maxDelay - pred.getValue)
  }
}
