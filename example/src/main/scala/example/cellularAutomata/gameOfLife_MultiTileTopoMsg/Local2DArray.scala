package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message

import scala.reflect.ClassTag
import scala.util.Random


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

  //padding
  object padding{
    var topLeft: V = _
    var topRight: V= _
    var bottomLeft: V= _
    var bottomRight: V= _
    var topEdge: Array[V]= Array.ofDim[V](w)
    var bottomEdge: Array[V]= Array.ofDim[V](w)
    var leftEdge: Array[V] = Array.ofDim[V](h)
    var rightEdge: Array[V] = Array.ofDim[V](h)
  }


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

  def updateMessage(c: LocalArray2D[V], buffer: Iterable[(Int,Vector[V])]): LocalArray2D[V]= {

    buffer.foreach {
      case (index, vec) => index match {
        case 0 => c.padding.topLeft = vec(0)
        case 1 => c.padding.topRight = vec(0)
        case 2 => c.padding.bottomLeft = vec(0)
        case 3 => c.padding.bottomRight = vec(0)
        case 4 => c.padding.topEdge =  vec.toArray
        case 5 => c.padding.bottomEdge=vec.toArray
        case 6 => c.padding.leftEdge=vec.toArray
        case 7 => c.padding.rightEdge=vec.toArray
        case _ => println("ERROR! Unexpected message type!")
      }
    }
    c

  }


//  def step(c: LocalArray2D[V]): LocalArray2D[V] = {
//
//  }





}