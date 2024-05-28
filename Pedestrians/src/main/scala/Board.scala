import java.awt.event.{ComponentEvent, ComponentListener, MouseEvent}
import java.awt.{Color, Graphics, Insets}
import javax.swing.JComponent
import javax.swing.event.MouseInputListener
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Board extends JComponent with MouseInputListener with ComponentListener {
  private var points: Array[Array[Point]] = _
  private val gridSize: Int = 10
  var editType: Int = 0
  private var neighborhood: Int = 1
  private val toCheck: ArrayBuffer[Point] = ArrayBuffer.empty[Point]
  private val SFMAX: Int = 100000

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
      points(x)(y).blocked = false
    }

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).move()
    }
    repaint()
  }

  // clearing board
  def clear(): Unit = {
    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).clear()
    }
    calculateField()
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
      x <- 1 until points.length - 1
      y <- 1 until points(x).length - 1
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      if !(i == x && j == y)
    } points(x)(y).addNeighbor(points(i)(j))

    for (x <- points.indices) {
      points(x)(0).types = 1
      points(x)(points(x).length - 1).types = 1
    }

    for (y <- points(0).indices) {
      points(0)(y).types = 1
      points(points.length - 1)(y).types = 1
    }
  }

  private def calculateField(): Unit = {
    for {
      pointRow <- points
      value <- pointRow
      if value.types == 2
    } {
      value.staticField = 0
      toCheck.addAll(value.neighbors)
    }

    while (toCheck.nonEmpty) {
      val poi = toCheck.remove(0)
      if (poi.calcStaticField()) {
        toCheck.addAll(poi.neighbors)
      }
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

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      val currentPoint = points(x)(y)
      currentPoint.types match {
        case 0 =>
          var staticField = currentPoint.staticField.toFloat
          var intensity = staticField / 100
          if (intensity > 1.0f) {
            intensity = 1.0f
          }
          g.setColor(new Color(intensity, intensity, intensity))
        case 1 => g.setColor(new Color(1.0f, 0.0f, 0.0f, 0.7f))
        case 2 => g.setColor(new Color(0.0f, 1.0f, 0.0f, 0.7f))
        case _ =>
          if (currentPoint.isPedestrian) {
            g.setColor(new Color(0.0f, 0.0f, 1.0f, 0.7f))
          }
      }
      g.fillRect(x * gridSize + 1, y * gridSize + 1, gridSize - 1, gridSize - 1)
    }
  }

  def mouseClicked(e: MouseEvent): Unit = {
    val x: Int = e.getX / gridSize
    val y: Int = e.getY / gridSize
    if (x < points.length && x > 0 && y < points(x).length && y > 0 && points(x)(y).types == 0) {
      if(editType==3){
        points(x)(y).isPedestrian = true
      }
      points(x)(y).types = editType
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
    if (x < points.length && x > 0 && y < points(x).length && y > 0 && points(x)(y).types == 0) {
      if (editType == 3) {
        points(x)(y).isPedestrian = true
      }
      points(x)(y).types = editType
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
