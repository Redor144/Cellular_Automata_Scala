import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class Point {
  private var neighbors: ArrayBuffer[Point] = ArrayBuffer[Point]()
  private var currentState: Int = 0
  private var nextState: Int = 0
  private val numStates: Int = 6


  def clicked(): Unit = {
    currentState = (currentState + 1) % numStates
  }

  def getState: Int = currentState

  def setState(s: Int): Unit = {
    currentState = s
  }

  def calculateNewState(): Unit = {
    val neighborNum: Int = countNeighbors()
    val toLive: mutable.HashSet[Int] = mutable.HashSet(2, 3)
    val toDie: mutable.HashSet[Int] = mutable.HashSet(3)

    if (getState == 1) {
      if (toLive.contains(neighborNum)) {
        nextState = 1
      } else {
        nextState = 0
      }
    } else {
      if (toDie.contains(neighborNum)) {
        nextState = 1
      } else {
        nextState = 0
      }
    }
  }

  def changeState(): Unit = {
    currentState = nextState
  }

  def addNeighbor(nei: Point): Unit = {
    neighbors += nei
  }

  private def countNeighbors(): Int = {
    neighbors.count(_.getState == 1)
  }
}
