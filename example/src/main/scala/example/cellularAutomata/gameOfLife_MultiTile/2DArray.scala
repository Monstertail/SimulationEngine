package example
package gameOfLifeMultiTile

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import squid.quasi.lift

import scala.util.Random
import scala.reflect.ClassTag

//@lift
class Array2D(val width: Int, val height: Int) {

  var tileNO: Int = _
//    val totalComp: Int = 1
//    implicit val ct: ClassTag[Boolean] = scala.reflect.classTag[Boolean]
  var currentBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)
  var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)
  //store neighbor information

  var i:Int=0
  var j:Int=0
  var aliveNeighbors:Int=0
  var ni:Int= -1
  var nj:Int= -1
  // initialize the arrays
  def init(tN:Int): Unit = {
    val random = new Random()
    for (i <- 0 until height; j <- 0 until width) {
      currentBoard(i)(j) = random.nextBoolean()
    }
    tileNO=tN
  }

  def packageMsg(): Vector[Vector[Boolean]] = {
    val topEdge: Vector[Boolean] = currentBoard(0).toVector
    val bottomEdge:Vector[Boolean] = currentBoard(height-1).toVector
    val leftEdge: Vector[Boolean] = currentBoard.map(row => row(0)).toVector
    val rightEdge: Vector[Boolean] = currentBoard.map(row => row(row.length-1)).toVector
    val topLeft:Vector[Boolean] = Vector(currentBoard(0)(0))
    val topRight:Vector[Boolean] = Vector(currentBoard(0)(width-1))
    val bottomLeft:Vector[Boolean] = Vector(currentBoard(height-1)(1))
    val bottomRight:Vector[Boolean] = Vector(currentBoard(height-1)(width-1))

    Vector(topLeft,leftEdge,bottomLeft,topEdge,bottomEdge,topRight,rightEdge,bottomRight).reverse

  }

  def decodeMsg(state: Vector[Boolean],mode:Int,from:Int,id:Int): Unit = {
    if(mode>=8){
      println("ERROR! TOO MANY MODES!")
    }
    else{

      println(s"mode $mode, length: ${state.length} from tile $from for tile $id with 2DArray of $tileNO")
    }

  }

  def update(): Unit = {
    i = 0
    while (i < height) {
      j = 0
      while (j < width) {
        aliveNeighbors = 0

        ni = -1
        while (ni <= 1) {
          nj = -1
          while (nj <= 1) {
            if (!(ni == 0 && nj == 0)) {
              val row = (i + ni + height) % height // handle wrapping at edges
              val col = (j + nj + width) % width // handle wrapping at edges
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
    printBooleanArray(newBoard)
  }

 def printBooleanArray(arr: Array[Array[Boolean]]): Unit = {
    for ( m <- arr.indices) {
      for (n <- arr(m).indices) {
        if (arr(m)(n)) print(1)
        else print(0)
      }
      println()
    }
 }

}