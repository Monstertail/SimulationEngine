package example
package gameOfLifeMultiTileTopoMsg
import meta.runtime.Message

sealed trait TopoMsg extends Message
case class VectorMsg[CompIdT,T](sid: CompIdT,v:IndexedSeq[T])  extends TopoMsg{
  def printMsg(): Unit = {
    println("VectorMsg: " + v)
  }
}

//case class RowMsgTop[T](v:IndexedSeq[T]) extends TopoMsg
//
//case class RowMsgBottom[T](v:IndexedSeq[T]) extends TopoMsg
//
//case class ColMsgLeft[T](v:IndexedSeq[T]) extends TopoMsg
//
//case class ColMsgRight[T](v:IndexedSeq[T]) extends TopoMsg
//
//case class DiagMsgTopLeft[T](v:T) extends TopoMsg
//
//case class DiagMsgTopRight[T](v:T) extends TopoMsg
//
//case class DiagMsgBottomLeft[T](v:T) extends TopoMsg
//
//case class DiagMsgBottomRight[T](v:T) extends TopoMsg