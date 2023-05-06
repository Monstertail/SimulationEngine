package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message
import scala.reflect.ClassTag


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


}