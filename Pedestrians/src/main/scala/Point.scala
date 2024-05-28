import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Point {
  var neighbors: ArrayBuffer[Point] = ArrayBuffer[Point]()
  var blocked: Boolean = false
  var types = 0;
  private val SFMAX: Int = 100_000
  var staticField: Int = SFMAX
  var isPedestrian: Boolean = false

//  def this() {
//    this()
//    currentState = 0
//    nextState = 0
//    neighbors = ArrayBuffer[Point]()
//  }
  def calcStaticField(): Boolean = {
    val smallestStaticField = neighbors.map(_.staticField).foldLeft(staticField)(Math.min)
    if (staticField > smallestStaticField + 1) {
      staticField = smallestStaticField + 1
      true
    } else {
      false
    }
  }

  def move(): Unit = {
    if (isPedestrian) {
      var smallest = Int.MaxValue
      var next: Point = null
      for (noi <- neighbors) {
        if (!noi.isPedestrian && noi.types != 1) {
          // smallest = Math.min(noi.staticField,smallest);
          if (smallest > noi.staticField) {
            smallest = noi.staticField
            next = noi
          }
        }
      }
      if (next != null && !blocked) {
        if (next.types == 0) {
          this.isPedestrian = false
          next.isPedestrian = true
          this.types = 0
          next.types = 3
          next.blocked = true
        } else if (next.types == 2) {
          this.isPedestrian = false
          this.types = 0
        }
      }
      // else if (next.type==2) {
      // this.isPedestrian=false;
      // this.type=0;
      // }
    }
  }

  def clear(): Unit = {
    staticField = SFMAX
  }

  def addNeighbor(nei: Point): Unit = {
    neighbors += nei
  }
}
