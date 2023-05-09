package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message

import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.mutable.ListBuffer

class gridCoordinate(val x: Int, val y: Int){
  val r_index=x
  val c_index=y
}



class LocalArray2D[V: scala.reflect.ClassTag](w:Int,h:Int,gid:gridCoordinate) extends AgentTopo[gridCoordinate]{
//  val global_id:Pair = new Pair(0,0)
//  val width: Int = 0
//  val height:Int = 0
  val width=w
  val height=h
  val global_id =gid
  var currentBoard: Array[Array[V]] = Array.ofDim[V](w, h)
  //newBoard is used to store the updated states after step function for every cell
  var newBoard: Array[Array[V]] = Array.ofDim[V](w, h)



  def tbs(c1:LocalArray2D[V],c2:LocalArray2D[V]): TopoMsg = {
    (c1.global_id.c_index, c1.global_id.r_index, c2.global_id.c_index, c2.global_id.r_index) match {
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r+c2.height == c1r  =>
        RowMsgTop(c1.currentBoard(0))
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r == c1r + c1.width =>
        RowMsgBottom(c1.currentBoard(c1.height - 1))
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c + c1.width == c2c =>
        ColMsgRight(c1.currentBoard.map(row => row(row.length - 1)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c  == c2c+c2.width =>
        ColMsgLeft(c1.currentBoard.map(row => row(0)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c  == c2c+c2.width =>
        DiagMsgTopLeft(c1.currentBoard(0)(0))
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c + c1.width == c2c =>
        DiagMsgTopRight(c1.currentBoard(0)(c1.width - 1))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c == c2c +c2.width=>
        DiagMsgBottomLeft(c1.currentBoard(c1.height - 1)(0))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c + c1.width == c2c =>
        DiagMsgBottomRight(c1.currentBoard(c1.height - 1)(c1.width - 1))

    }

  }

  def processMessage(m:TopoMsg,c:LocalArray2D[V]):(Int,Vector[V]) ={
    m match {
      case DiagMsgBottomRight(v) => {
        (0,Vector(v.asInstanceOf[V]))
      }
      case DiagMsgBottomLeft(v) => {
        (1, Vector(v.asInstanceOf[V]))
      }
      case DiagMsgTopRight(v) => {
        (2, Vector(v.asInstanceOf[V]))
      }
      case DiagMsgTopLeft(v) => {
        (3, Vector(v.asInstanceOf[V]))
      }
      case RowMsgBottom(v) => {
        (4, v.toVector.asInstanceOf[Vector[V]])
      }
      case RowMsgTop(v) => {

        (5, v.toVector.asInstanceOf[Vector[V]])
      }
      case ColMsgRight(v) => {
        (6, v.toVector.asInstanceOf[Vector[V]])
      }

      case ColMsgLeft(v) => {
        (7, v.toVector.asInstanceOf[Vector[V]])
      }


    }


  }


  def updateComponent(buffer:Iterable[(Int,Vector[V])],perCellAct:(V,Iterable[V])=>V):Unit = {

    val receiveMessage=buffer

    for (i<-0 until  height) {

      for (j<-0 until  width) {
        val  neighbors= ListBuffer[V]()
        for (ni<- -1 to  1) {

          for (nj <- -1 to 1) {
            if (!(ni == 0 && nj == 0)) {
              //check the location of neighbors and update
              //row index of the neighbor
              val r = i + ni
              //column index of the neighbor
              val c = j + nj

              if (r == -1) {
                if (c == -1) {
                  //topLeft
                  neighbors+=receiveMessage.find { case (i, v) => i == 0 }.map { case (i, v) => v(0) }
                }
                else if (c == width) {
                  //topRight
                  neighbors+=receiveMessage.find { case (i, v) => i == 1 }.map { case (i, v) => v(0) }
                }

                else {
                  //topEdge(c)
                  neighbors+=receiveMessage.find { case (i, v) => i == 4 }.map { case (i, v) => v(c) }
                }
              } else if (r == height) {
                if (c == -1) {
                  // bottomLeft
                  neighbors+=receiveMessage.find { case (i, v) => i == 2 }.map { case (i, v) => v(0) }

                }
                else if (c == width) {
                  //bottomRight
                  neighbors+=receiveMessage.find { case (i, v) => i == 3 }.map { case (i, v) => v(0) }
                }
                else {
                  //bottomEdge(c)
                  neighbors+=receiveMessage.find { case (i, v) => i == 5 }.map { case (i, v) => v(c) }
                }
              } else {
                if (c == -1) {
                  //leftEdge(r)
                  neighbors+=receiveMessage.find { case (i, v) => i == 6 }.map { case (i, v) => v(r) }
                } else if (c == width) {
                  //rightEdge(r)
                  neighbors+=receiveMessage.find { case (i, v) => i == 7 }.map { case (i, v) => v(r) }
                } else {
                  neighbors+= currentBoard(r)(c)
                }
              }

            }

          }

        }

        // step function:apply the game of life rules to determine the next state

        newBoard(i)(j)=perCellAct(currentBoard(i)(j),neighbors)

      }

    }

    // swap the reference

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








}