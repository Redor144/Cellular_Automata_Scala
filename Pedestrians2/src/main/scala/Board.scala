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
//  private val toCheck: ArrayBuffer[Point] = ArrayBuffer.empty[Point]
  private var iterationCounter: Int = 0
  private val DFMAX: Int = 100_000
  private val WALLRF: Int = 30
  private val EXIT2RATIO: Int = 3
  private val EXIT4RATIO: Int = 5;


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
    } {//TODO random move
      points(x)(y).move()
    }
    if(iterationCounter % 15 == 0){
      calcDynamicField();
    }
    iterationCounter+=1;
    repaint()
  }

  def calcDynamicField(): Unit = {
    var min_exit_counter = iterationCounter+100
    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y).dynamicField = DFMAX
      if(points(x)(y).types == 2 || points(x)(y).types == 4){
        min_exit_counter = Math.min(min_exit_counter,points(x)(y).exit_pedestrians_counter)
      }
    }
    val toCheckField: ArrayBuffer[Point] = ArrayBuffer.empty[Point]
    for {
      x <- 1 until points.length - 1
      y <- 1 until points(x).length - 1
    } {
      if (points(x)(y).types == 2) {
        points(x)(y).dynamicField = EXIT2RATIO * (points(x)(y).exit_pedestrians_counter - min_exit_counter)
        toCheckField.addAll(points(x)(y).neighbors)
      }
      if (points(x)(y).types == 4) {
        points(x)(y).dynamicField = EXIT4RATIO * (points(x)(y).exit_pedestrians_counter - min_exit_counter)
        toCheckField.addAll(points(x)(y).neighbors)
      }
    }
    while (toCheckField.nonEmpty) {
      val currPoint = toCheckField.head
      if (currPoint.calcDynamicField()) {
        toCheckField.addAll(currPoint.neighbors)
      }
      toCheckField.remove(0)
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
    calcDynamicField()
    repaint()
  }

  private def initialize(length: Int, height: Int): Unit = {
    var MooreNH = true

    points = Array.ofDim[Point](length, height)

    for {
      x <- points.indices
      y <- points(x).indices
    } {
      points(x)(y) = new Point()
      if (x == 0 || y == 0 || x == points.length - 1 || y == points(x).length - 1) {
        points(x)(y).types = 1
      }
    }

    for {
      x <- 1 until points.length - 1
      y <- 1 until points(x).length - 1
    } {
      if (MooreNH) {
        points(x)(y).addNeighbor(points(x - 1)(y - 1))
        points(x)(y).addNeighbor(points(x + 1)(y - 1))
        points(x)(y).addNeighbor(points(x + 1)(y + 1))
        points(x)(y).addNeighbor(points(x - 1)(y + 1))
      }
      points(x)(y).addNeighbor(points(x)(y - 1))
      points(x)(y).addNeighbor(points(x + 1)(y))
      points(x)(y).addNeighbor(points(x)(y + 1))
      points(x)(y).addNeighbor(points(x - 1)(y))
    }
  }

  private def calculateField(): Unit = {
    val toCheckForce: ArrayBuffer[Point] = ArrayBuffer.empty[Point]
    val toCheckField: ArrayBuffer[Point] = ArrayBuffer.empty[Point]

    for {
      pointRow <- points
      value <- pointRow
    } {
      if (value.types == 2 || value.types == 4) {
        value.staticField = 0
        toCheckField.addAll(value.neighbors)
      }
      if (value.types == 1) {
        value.wallRepulsionForce = WALLRF
        toCheckForce.addAll(value.neighbors)
      }
    }

    while (toCheckForce.nonEmpty) {
      val currPoint = toCheckForce.head
      if (currPoint.calcRepulsionForce()) {
        toCheckForce.addAll(currPoint.neighbors)
      }
      toCheckForce.remove(0)
    }

    for {
      pointRow <- points
      value <- pointRow
    } {
      value.staticField += value.wallRepulsionForce
    }

    while (toCheckField.nonEmpty) {
      val currPoint = toCheckField.head
      if (currPoint.calcStaticField()) {
        toCheckField.addAll(currPoint.neighbors)
      }
      toCheckField.remove(0)
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
          var dynamicField = currentPoint.dynamicField.toFloat
          var intensity = (staticField + dynamicField) / 2000
          if (intensity > 1.0f) {
            intensity = 1.0f
          }
          g.setColor(new Color(intensity, intensity, intensity))
        case 1 => g.setColor(new Color(1.0f, 0.0f, 0.0f, 0.7f))
        case 2 => g.setColor(new Color(0.0f, 1.0f, 0.0f, 0.7f))
        case 4 => g.setColor(new Color(1.0f, 0.8f, 0.0f, 0.7f))
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
        points(x)(y).makePedestrian()
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
        points(x)(y).makePedestrian()
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
