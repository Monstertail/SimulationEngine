package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message


class Pair(val x: Int, val y: Int){
  val r_index=x
  val c_index=y
}

class LocalArray2D[V](width:Int,height:Int,global_id:Pair) extends AgentTopo[Pair]{
//  val global_id:Pair = new Pair(0,0)
//  val width: Int = 0
//  val height:Int = 0
  val w=width
  val h=height
  var currentBoard: Array[Array[V]] = Array.ofDim[V](height, width)
  var newBoard: Array[Array[V]] = Array.ofDim[V](height, width)

  def tbs(c1:LocalArray2D[V],c2:LocalArray2D[V]): Message = {
    case (c1.global_id.c_index == c2.global_id.c_index) && (c2.global_id.r_index==c1.global_id.c_index-1)=>{
      val msg= RowMsgGoUp(c1.currentBoard(0))
      msg
    }
    case (c1.global_id.c_index == c2.global_id.c_index) && (c2.global_id.r_index==c1.global_id.c_index+c1.width)=>{
      val msg=RowMsgGoDown(c1.currentBoard(c1.height-1))
      msg
    }
    case (c1.global_id.r_index==c2.global_id.r_index) &&(c1.global_id.c_index+c1.width==c2.global_id.c_index)=>{
      val msg = ColMsgGoRight(c1.currentBoard.map(row => row(row.length-1)))
      msg
    }

    case (c1.global_id.r_index == c2.global_id.r_index) && (c1.global_id.c_index -1 == c2.global_id.c_index) => {
      val msg = ColMsgGoLeft(c1.currentBoard.map(row => row(0)))
      msg
    }

    case (c1.global_id.r_index == c2.global_id.r_index+c2.height) && (c1.global_id.c_index - 1 == c2.global_id.c_index) => {
      val msg = DiagMsgTopLeft(c1.currentBoard(0)(0))
      msg
    }

    case (c1.global_id.r_index == c2.global_id.r_index+c2.height) && (c1.global_id.c_index + c1.width == c2.global_id.c_index) => {
      val msg = DiagMsgTopRight(c1.currentBoard(0)(c1.width-1))
      msg
    }

    case (c1.global_id.r_index+c1.height == c2.global_id.r_index) && (c1.global_id.c_index - 1 == c2.global_id.c_index) => {
      val msg = DiagMsgBottomLeft(c1.currentBoard(c1.height-1)(0))
      msg
    }

    case (c1.global_id.r_index+c1.height  == c2.global_id.r_index) && (c1.global_id.c_index + c1.width == c2.global_id.c_index) => {
      val msg = DiagMsgBottomRight(c1.currentBoard(c1.height-1)(c1.width - 1))
      msg
    }



  }




}