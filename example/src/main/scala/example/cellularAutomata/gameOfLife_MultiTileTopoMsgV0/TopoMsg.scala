package example
package gameOfLifeMultiTileTopoMsgV0
import meta.runtime.Message

sealed trait TopoMsg extends Message

case class RowMsgTop[T](v:IndexedSeq[T]) extends TopoMsg

case class RowMsgBottom[T](v:IndexedSeq[T]) extends TopoMsg

case class ColMsgLeft[T](v:IndexedSeq[T]) extends TopoMsg

case class ColMsgRight[T](v:IndexedSeq[T]) extends TopoMsg

case class DiagMsgTopLeft[T](v:T) extends TopoMsg

case class DiagMsgTopRight[T](v:T) extends TopoMsg

case class DiagMsgBottomLeft[T](v:T) extends TopoMsg

case class DiagMsgBottomRight[T](v:T) extends TopoMsg