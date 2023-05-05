package example
package gameOfLifeMultiTileTopoMsg
import meta.runtime.Message

sealed trait TopoMsg extends Message

case class RowMsgGoUp[T](v:Array[T]) extends TopoMsg

case class RowMsgGoDown[T](v:Array[T]) extends TopoMsg

case class ColMsgGoLeft[T](v:Array[T]) extends TopoMsg

case class ColMsgGoRight[T](v:Array[T]) extends TopoMsg

case class DiagMsgTopLeft[T](v:T) extends TopoMsg

case class DiagMsgTopRight[T](v:T) extends TopoMsg

case class DiagMsgBottomLeft[T](v:T) extends TopoMsg

case class DiagMsgBottomRight[T](v:T) extends TopoMsg