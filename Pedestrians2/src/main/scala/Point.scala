import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Point {
  var neighbors: ArrayBuffer[Point] = ArrayBuffer[Point]()
  var blocked: Boolean = false
  var types = 0;
  private val SFMAX: Int = 100_000
  private val DFMAX: Int = 100_000
  var staticField: Int = SFMAX
  var isPedestrian: Boolean = false
  var wallRepulsionForce: Int = 0
  var exit_pedestrians_counter: Int = 0
  var dynamicField: Int = 0

//  def this() {
//    this()
//    currentState = 0
//    nextState = 0
//    neighbors = ArrayBuffer[Point]()
//  }
def calcRepulsionForce(): Boolean = {
  var maxRepulsionForce = 0
  if (neighbors.nonEmpty) {
    for (neighbor <- neighbors)
      maxRepulsionForce = math.max(maxRepulsionForce, neighbor.wallRepulsionForce)
    // maxRepulsionForce = neighbors.map(_.wallRepulsionForce).maxOption.getOrElse(0)
  }
  if (wallRepulsionForce < maxRepulsionForce - 10) {
    wallRepulsionForce = maxRepulsionForce - 10
    true
  } else {
    false
  }
}

  def calcDynamicField(): Boolean = {
    var minDynamicField = DFMAX
    var field_increase = 10

    if (neighbors.nonEmpty) {
      val minDynamicFieldPoint = neighbors.minBy(_.dynamicField)
      if (neighbors.lastIndexOf(minDynamicFieldPoint) < 4)
        field_increase = 14

      minDynamicField = minDynamicFieldPoint.dynamicField
    }

    if (dynamicField > minDynamicField + field_increase + wallRepulsionForce) {
      dynamicField = minDynamicField + field_increase + wallRepulsionForce
      true
    } else {
      false
    }
  }

  def calcStaticField(): Boolean = {
  var minStaticField = SFMAX
  var field_increase = 10

  if (neighbors.nonEmpty) {
    val minStaticFieldPoint = neighbors.minBy(_.staticField)
    if (neighbors.lastIndexOf(minStaticFieldPoint) < 4)
      field_increase = 14

    minStaticField = minStaticFieldPoint.staticField
  }

  if (staticField > minStaticField + field_increase + wallRepulsionForce) {
    staticField = minStaticField + field_increase + wallRepulsionForce
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
          if (smallest > noi.staticField-dynamicField) {
            smallest = noi.staticField-dynamicField
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
        } else if (next.types == 2 || next.types == 4) {
          this.isPedestrian = false
          this.types = 0
          next.exit_pedestrians_counter +=1
        }
      }
      else if(next==null){
        blocked = true
      }
      // else if (next.type==2) {
      // this.isPedestrian=false;
      // this.type=0;
      // }
    }
//    if (isPedestrian && neighbors.nonEmpty && !blocked) {
//      var MinSFNNeighbor: Point = neighbors.head
//      for (neighbor <- neighbors) {
//        println(neighbor.dynamicField,neighbor.staticField)
////        if (!neighbor.isPedestrian && neighbor.types != 1 && !neighbor.blocked) {
////          if (neighbor.staticField - neighbor.dynamicField < MinSFNNeighbor.staticField - MinSFNNeighbor.dynamicField) {
////            MinSFNNeighbor = neighbor
////            println(MinSFNNeighbor.staticField-dynamicField)
////          }
////        }
//      }
//      if(!MinSFNNeighbor.isPedestrian && MinSFNNeighbor.types != 1 && !MinSFNNeighbor.blocked){
//        MinSFNNeighbor.types match{
//          case 0 =>
//            MinSFNNeighbor.isPedestrian = true
//            MinSFNNeighbor.types = 3
//            MinSFNNeighbor.blocked = true
//            //              removePedestrian()
//            isPedestrian = false
//            types = 0
//          case 2 | 4 =>
//            isPedestrian = false
//            types = 0
//            MinSFNNeighbor.exit_pedestrians_counter += 1
//        }
//      }
//      else{
//        blocked = true
//      }
//    }
  }

  def clear(): Unit = {
    staticField = SFMAX
    wallRepulsionForce = 0;
    exit_pedestrians_counter = 0;
    dynamicField = 0;
  }

  def addNeighbor(nei: Point): Unit = {
    neighbors += nei
  }
  def makePedestrian(): Unit ={
    if(!isPedestrian){
      isPedestrian = true
    }
  }
  def removePedestrian(): Unit = {
    if (isPedestrian) {
      isPedestrian = false
    }
  }
}
