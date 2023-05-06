package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import squid.quasi.lift

import scala.util.Random
import scala.reflect.ClassTag
import  scala.collection.immutable.Vector


//define MsgType
//class myMsgType extends Message{
//
//}

//@lift
class Array2DCal(val L2DA:LocalArray2D[Boolean]) {

  var tileNO: Int = _
  val width=L2DA.width
  val height=L2DA.height

//  var currentBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)
//  var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

  //store neighbor information
  var topEdge: Array[Boolean]=Array.ofDim[Boolean]( width)
  var bottomEdge: Array[Boolean]=Array.ofDim[Boolean]( width)
  var leftEdge: Array[Boolean]=Array.ofDim[Boolean]( height)
  var rightEdge: Array[Boolean]=Array.ofDim[Boolean]( height)
  var topLeft:Boolean = _
  var topRight:Boolean = _
  var bottomLeft:Boolean = _
  var bottomRight:Boolean = _

  var i:Int=0
  var j:Int=0
  var aliveNeighbors:Int=0
  var ni:Int= -1
  var nj:Int= -1
  // initialize the arrays
  def init(tN:Int): Unit = {
    val random = new Random()
    for (i <- 0 until height; j <- 0 until width) {
      L2DA.currentBoard(i)(j) = random.nextBoolean()
    }
    tileNO=tN
  }


  def decodeMsg(m: TopoMsg): Unit = {
    m match {
      case RowMsgTop(arr) => {
//        println(arr)
//        println(arr.toVector)
        bottomEdge = arr.toVector.asInstanceOf[Vector[Boolean]].toArray
      }

      case RowMsgBottom(arr) => {
        topEdge = arr.toVector.asInstanceOf[Vector[Boolean]].toArray
      }

      case ColMsgRight(arr) => {
        leftEdge = arr.toVector.asInstanceOf[Vector[Boolean]].toArray
      }

      case ColMsgLeft(arr) => {
        rightEdge = arr.toVector.asInstanceOf[Vector[Boolean]].toArray
      }

      case DiagMsgTopLeft(v) => {
        bottomRight = v.asInstanceOf[Boolean]
      }

      case DiagMsgTopRight(v) => {
        bottomLeft = v.asInstanceOf[Boolean]
      }

      case DiagMsgBottomLeft(v) => {
        topRight = v.asInstanceOf[Boolean]
      }

      case DiagMsgBottomRight(v) => {
        topLeft = v.asInstanceOf[Boolean]
      }


    }
  }



  def locationQuery(r:Int,c:Int): Boolean = {
      if(r== -1){
        if (c == -1){
          return topLeft
        }
        else if (c == width){
          return  topRight
        }
        else{
          return topEdge(c)
        }
      } else if (r==height){
        if (c == -1) {
          return bottomLeft
        }
        else if (c == width ) {
          return bottomRight
        }
        else {
          return bottomEdge(c)
        }
      } else{
        if (c == -1){
          return leftEdge(r)
        }else if(c==width){
          return rightEdge(r)
        } else{
          return L2DA.currentBoard(r)(c)
        }
      }
  }

// -------------------------step function()
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
//              val row = (i + ni + height) % height // handle wrapping at edges
//              val col = (j + nj + width) % width // handle wrapping at edges
//              if (currentBoard(row)(col)) aliveNeighbors = aliveNeighbors + 1
              //check the location and update
              val row = i + ni
              val col = j + nj
              if (locationQuery(row,col)) aliveNeighbors = aliveNeighbors + 1
            }
            nj = nj + 1
          }
          ni = ni + 1
        }

        // apply the game of life rules to determine the next state
        if (L2DA.currentBoard(i)(j)) {
          if (aliveNeighbors == 2 || aliveNeighbors == 3) {
            L2DA.newBoard(i)(j) = true
          } else {
            L2DA.newBoard(i)(j) = false
          }
        } else {
          if (aliveNeighbors == 3) {
            L2DA.newBoard(i)(j) = true
          } else {
            L2DA.newBoard(i)(j) = false
          }
        }

        j = j + 1
      }
      i = i + 1
    }

  }

  // -------------------------swap reference for double buffering
  def swap_ref():Unit={
    // Swap the buffers
    val temp = L2DA.currentBoard
    // check if two references point to the same object in memory
    // If isSameArray is false, then a new array was created and the pointers were not simply swapped.
    val isSameArray = L2DA.currentBoard eq temp

    L2DA.currentBoard = L2DA.newBoard
    val CNSameArray = L2DA.currentBoard eq L2DA.newBoard

    L2DA.newBoard = temp
    val NTSameArray = temp eq L2DA.newBoard

    if (!(isSameArray && CNSameArray && NTSameArray)) {
      println(s"ERROR:two references point to the different objects in memory!")
    }

  }

  //-------------------------------correctness test
 def check():Unit={
    printBooleanArray(L2DA.newBoard)
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
//---------------------------generate Msgs to be sent



}