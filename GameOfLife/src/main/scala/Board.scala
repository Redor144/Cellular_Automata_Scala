import java.awt.{Color, Graphics, Insets}
import java.awt.event.{ComponentEvent, ComponentListener, MouseEvent}
import javax.swing.JComponent
import javax.swing.event.MouseInputListener

class Board extends JComponent with MouseInputListener with ComponentListener {
  private var points: Array[Array[Point]] = _
  private val gridSize: Int = 14

  addMouseListener(this)
  addComponentListener(this)
  addMouseMotionListener(this)
  setBackground(Color.WHITE)
  setOpaque(true)

  def this(width: Int, height: Int){
    this()
    initialize(width,height)
  }

  // single iteration
  def iteration(): Unit = {
    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).calculateNewState()
    }

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).changeState()
    }
    repaint()
  }

  // clearing board
  def clear(): Unit = {
    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).setState(0)
    }
    repaint()
  }

  private def initialize(length: Int, height: Int): Unit = {
    points = Array.ofDim[Point](length, height)

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y) = new Point()
    }

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      addNeighbours(x, y)
    }
  }

  private def addNeighbours(x: Int, y: Int): Unit = {
    for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      if i != x || j != y
      if i > -1 && i < points.length && j > -1 && j < points(x).length
    } {
      points(x)(y).addNeighbor(points(i)(j))
    }
  }

  //paint background and separators between cells
  override protected def paintComponent(g: Graphics): Unit = {
    if (isOpaque) {
      g.setColor(getBackground)
      g.fillRect(0, 0, getWidth, getHeight)
    }
    g.setColor(Color.GRAY)
    drawNetting(g, gridSize)

    for {
      x <- points.indices
      y <- points(x).indices
      if points(x)(y).getState != 0
    } {
      points(x)(y).getState match {
        case 1 => g.setColor(new Color(0x0000ff))
        case 2 => g.setColor(new Color(0x00ff00))
        case 3 => g.setColor(new Color(0xff0000))
        case 4 => g.setColor(new Color(0x000000))
        case 5 => g.setColor(new Color(0x444444))
        case 6 => g.setColor(new Color(0xffffff))
        case _ => g.setColor(Color.BLACK)
      }
      g.fillRect(x * gridSize + 1, y * gridSize + 1, gridSize - 1, gridSize - 1)
    }
  }

  // draws the background netting
  private def drawNetting(g: Graphics, gridSpace: Int): Unit = {
    val insets: Insets = getInsets
    val firstX: Int = insets.left
    val firstY: Int = insets.top
    val lastX: Int = getWidth - insets.right
    val lastY: Int = getHeight - insets.bottom
    var x: Int = firstX
    while (x < lastX) {
      g.drawLine(x, firstY, x, lastY)
      x += gridSpace
    }
    var y: Int = firstY
    while (y < lastY) {
      g.drawLine(firstX, y, lastX, y)
      y += gridSpace
    }
  }

  def mouseClicked(e: MouseEvent): Unit = {
    val x: Int = e.getX / gridSize
    val y: Int = e.getY / gridSize
    if (x < points.length && x > 0 && y < points(x).length && y > 0) {
      points(x)(y).clicked()
      repaint()
    }
  }

  def componentResized(e: ComponentEvent): Unit = {
    val dlugosc: Int = getWidth / gridSize + 1
    val wysokosc: Int = getHeight / gridSize + 1
    initialize(dlugosc, wysokosc)
  }

  def mouseDragged(e: MouseEvent): Unit = {
    val x: Int = e.getX / gridSize
    val y: Int = e.getY / gridSize
    if (x < points.length && x > 0 && y < points(x).length && y > 0) {
      points(x)(y).setState(1)
      repaint()
    }
  }

  def mouseExited(e: MouseEvent): Unit = {}

  def mouseEntered(e: MouseEvent): Unit = {}

  def componentShown(e: ComponentEvent): Unit = {}

  def componentMoved(e: ComponentEvent): Unit = {}

  def mouseReleased(e: MouseEvent): Unit = {}

  def mouseMoved(e: MouseEvent): Unit = {}

  def componentHidden(e: ComponentEvent): Unit = {}

  def mousePressed(e: MouseEvent): Unit = {}
}
