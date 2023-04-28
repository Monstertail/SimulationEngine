package example
package gameOfLifeMultiTilePreA

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import squid.quasi.lift

import scala.util.Random
import scala.reflect.ClassTag

//@lift
class Array2D(val width: Int, val height: Int) {

  var tileNO: Int = _
  var currentBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height+2, width+2)
  var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height+2, width+2)

//  //store neighbor information
//  var topEdge: Array[Boolean]=Array.ofDim[Boolean]( width)
//  var bottomEdge: Array[Boolean]=Array.ofDim[Boolean]( width)
//  var leftEdge: Array[Boolean]=Array.ofDim[Boolean]( height)
//  var rightEdge: Array[Boolean]=Array.ofDim[Boolean]( height)
//  var topLeft:Boolean = _
//  var topRight:Boolean = _
//  var bottomLeft:Boolean = _
//  var bottomRight:Boolean = _

  var i:Int=0
  var j:Int=0
  var aliveNeighbors:Int=0
  var ni:Int= -1
  var nj:Int= -1
  // initialize the arrays
  def init(tN:Int): Unit = {
    val random = new Random()
    for (i <- 0 until height+2; j <- 0 until width+2) {
      currentBoard(i)(j) = random.nextBoolean()
    }
    tileNO=tN
  }

  def packageMsg(): Vector[Vector[Boolean]] = {
    val topEdge: Vector[Boolean] = currentBoard(1).slice(1,width+1).toVector
    val bottomEdge:Vector[Boolean] = currentBoard(height).slice(1,width+1).toVector
    val leftEdge: Vector[Boolean] = (1 until currentBoard.length - 1).map(row => currentBoard(row)(1)).toVector
    val rightEdge: Vector[Boolean] = (1 until currentBoard.length - 1).map(row => currentBoard(row)(width)).toVector

    val topLeft:Vector[Boolean] = Vector(currentBoard(1)(1))
    val topRight:Vector[Boolean] = Vector(currentBoard(1)(width))
    val bottomLeft:Vector[Boolean] = Vector(currentBoard(height)(1))
    val bottomRight:Vector[Boolean] = Vector(currentBoard(height)(width))

    Vector(topLeft,leftEdge,bottomLeft,topEdge,bottomEdge,topRight,rightEdge,bottomRight).reverse

  }

  def decodeMsg(state: Vector[Boolean],direction:Int,from:Int,id:Int): Unit = {
    if(direction>=8){
      println("Direction TOTAL NUMBER ERROR !")
    }
    //debug
//    else{
//
//      println(s"direction $direction, length: ${state.length} from tile $from for tile $id with 2DArray of $tileNO")
//    }
    direction match{
      case 0=>{
        //from the neighbor's bottom right
        //topLeft
        currentBoard(0)(0)=state(0)

      }
      case 1=>{
        //from the neighbor's right
        //leftEdge
        if(state.length !=height){
          println("Direction 1 Vector length ERROR!")
        }
        for (row <- 1 until state.length+1) {
          currentBoard(row)(0) = state(row-1)
        }
      }

      case 2=>{
        //from the neighbor's  top Right
        //bottomLeft
        currentBoard(height+1)(0)=state(0)
      }

      case 3=>{
        //from the neighbor's  bottom Edge
        //topEdge
        if (state.length != width) {
          println(s"Direction 3 Vector length ${state.length} ERROR!")
        }
        for (col <- 1 until state.length+1) {
          currentBoard(0)(col) = state(col-1)
        }
      }

      case 4=>{
        //from the neighbor's top Edge
        //bottomEdge
        if (state.length != width) {
          println(s"Direction 4 Vector length ${state.length} ERROR!")
        }
        for (col <- 1 until state.length + 1) {
          currentBoard(height+1)(col) = state(col-1)
        }
      }

      case 5=>{
        //from the neighbor's bottom left
        //topRight
        currentBoard(0)(width+1)=state(0)
      }

      case 6=>{
        //from the neighbor's left Edge
        //rightEdge
        if (state.length != height) {
          println("Direction 6 Vector length ERROR!")
        }
        for (row <- 1 until state.length + 1) {
          currentBoard(row)(width+1) = state(row-1)
        }
      }

      case 7=>{
        //from the neighbor's topleft
        //bottomRight
        currentBoard(height+1)(width+1)=state(0)
      }
    }

  }



  def update(): Unit = {
    i = 1
    while (i < height+1) {
      j = 1
      while (j < width+1) {
        aliveNeighbors = 0

        ni = -1
        while (ni <= 1) {
          nj = -1
          while (nj <= 1) {
            if (!(ni == 0 && nj == 0)) {
//              val row = (i + ni + height) % height // handle wrapping at edges
//              val col = (j + nj + width) % width // handle wrapping at edges
              val row = i + ni  // handle wrapping at edges
              val col = j + nj
              if (currentBoard(row)(col)) aliveNeighbors = aliveNeighbors + 1

            }
            nj = nj + 1
          }
          ni = ni + 1
        }

        // apply the game of life rules to determine the next state
        if (currentBoard(i)(j)) {
          if (aliveNeighbors == 2 || aliveNeighbors == 3) {
            newBoard(i)(j) = true
          } else {
            newBoard(i)(j) = false
          }
        } else {
          if (aliveNeighbors == 3) {
            newBoard(i)(j) = true
          } else {
            newBoard(i)(j) = false
          }
        }

        j = j + 1
      }
      i = i + 1
    }

  }


  def swap_ref():Unit={
    // Swap the buffers
    val temp = currentBoard
    // check if two references point to the same object in memory
    // If isSameArray is false, then a new array was created and the pointers were not simply swapped.
    val isSameArray = currentBoard eq temp

    currentBoard = newBoard
    val CNSameArray = currentBoard eq newBoard

    newBoard = temp
    val NTSameArray = temp eq newBoard

    if (!(isSameArray && CNSameArray && NTSameArray)) {
      println(s"ERROR:two references point to the different objects in memory!")
    }

  }

 def check():Unit={
//    printBooleanArray(newBoard)
   printBooleanArray(currentBoard)
  }

 def printBooleanArray(arr: Array[Array[Boolean]]): Unit = {
   println(s"tile id: $tileNO")
    for ( m <- arr.indices) {
      for (n <- arr(m).indices) {
        if (arr(m)(n)) print(1)
        else print(0)
      }
      println()
    }
 }

}