package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import squid.quasi.lift

import scala.util.Random
import scala.reflect.ClassTag

class Pair(val x: Int, val y: Int){
  val r_index=x
  val c_index=y
}

//define MsgType
//class myMsgType extends Message{
//
//}

//@lift
class Array2D(val width: Int, val height: Int,val global_Tid:Pair) extends AgentTopo[Pair] {

  var tileNO: Int = _
  var currentBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)
  var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

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
      currentBoard(i)(j) = random.nextBoolean()
    }
    tileNO=tN
  }


  def encodeMsg(encode:Int): Message = {
    val pack=new packMsg
    pack.tbs(encode, new Pair(0, 0), new Pair(0, 0))

  }



  def decodeMsg(m:Message): Unit = {

    //debug
//    else{
//
//      println(s"direction $direction, length: ${state.length} from tile $from for tile $id with 2DArray of $tileNO")
//    }
     m.encode match{
      case 0=>{
        //from the neighbor's bottom right
        topLeft=m.state(0)

      }
      case 1=>{
        //from the neighbor's right
        leftEdge=m.state.toArray
      }

      case 2=>{
        //from the neighbor's  top Right
        bottomLeft=m.state(0)
      }

      case 3=>{
        //from the neighbor's  bottom Edge
        topEdge=m.state.toArray
      }

      case 4=>{
        //from the neighbor's top Edge
        bottomEdge=m.state.toArray
      }

      case 5=>{
        //from the neighbor's bottom left
        topRight=m.state(0)
      }

      case 6=>{
        //from the neighbor's left Edge
        rightEdge=m.state.toArray
      }

      case 7=>{
        //from the neighbor's topleft
        bottomRight=m.state(0)
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
          return currentBoard(r)(c)
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

  // -------------------------swap reference for double buffering
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

  //-------------------------------correctness test
 def check():Unit={
    printBooleanArray(newBoard)
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
//  class rowMsg extends super.Msg {
//    override def tbs(id1: Pair, id2: Pair): Message = {
//      val msg = new Message()
//      // perform some operations using id1 and id2
//      msg.encode=1
//      msg.value=if (currentBoard(id1.r_index)(id1.c_index)) 1 else 0
//      msg
//    }
//  }

  class packMsg extends super.Msg {
    override def tbs(encode:Int,index1:Pair,dst:Pair): Message = {
      val msg = new Message()
      // perform some operations using id1 and id2
      msg.encode = encode
//      msg.value = if (currentBoard(index1.r_index)(index1.c_index)) 1 else 0
      encode match {
        case 0 => {
          //src: bottom Right dst: top Left
          msg.state=Vector(currentBoard(height-1)(width-1))
        }

        case 1 => {
          //src: rightEdge
          msg.state=currentBoard.map(row => row(row.length-1)).toVector

        }

        case 2=>{
          //src: topRight
          msg.state=Vector(currentBoard(0)(width-1))

        }

        case 3=>{
          // src: bottomEdge
          msg.state=currentBoard(height-1).toVector

        }

        case 4=>{
          //src:topEdge
          msg.state=currentBoard(0).toVector
        }

        case 5=>{
          //src:bottomLeft
          msg.state=Vector(currentBoard(height-1)(0))
        }

        case 6=>{
          //src: leftEdge
          msg.state=currentBoard.map(row => row(0)).toVector
        }

        case 7=>{
          //src: topleft
          msg.state= Vector(currentBoard(0)(0))
        }


      }

      msg
    }
  }


}