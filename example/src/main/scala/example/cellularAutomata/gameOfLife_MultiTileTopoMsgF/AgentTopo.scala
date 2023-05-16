package example
package gameOfLifeMultiTileTopoMsgF

import meta.runtime.Actor.AgentId
import meta.runtime.{Actor, Message}
import scala.collection.mutable
class AgentTopo[CompIdT, IncMsgBuffT]{
  //topo of agents
  var Atopo:IndexedSeq[Actor]= _
  // topo of components.Record the neighbor components.
  var compNeighborList: IndexedSeq[AgentTopo[CompIdT, IncMsgBuffT]] = _
  // record the component ID
  var CompId:CompIdT = _



  /* run at sim initialization time, not during a step
     generates a function to execute at runtime
  */
  type SndFN = () => TopoMsg
  type RcvFN = TopoMsg => Unit

  val sndHndlr:mutable.Map[CompIdT, SndFN] = mutable.Map.empty


  val rcvHndlr: mutable.Map[CompIdT, RcvFN] = mutable.Map.empty



  // make send and rcv handlers ; call tbs and tbr
  def init(): Unit = {
    compNeighborList.foreach(n => {
      //update sender handler
      sndHndlr.update(n.CompId, tbs(this, n))
      //update receive handler
      rcvHndlr.update(n.CompId, tbr(n, this))
    })
  }

//  def tbs(Sid:CompIdT,Did:CompIdT) : SndFN = ???
//  def tbr(Sid:CompIdT,Did:CompIdT) : RcvFN = ???

  //For tbs and tbr, we should use component instead of component id as the parameter because some information
  // like the height and width of the component array is also required inside tbs and tbr.
  def tbs(S: AgentTopo[CompIdT, IncMsgBuffT], D: AgentTopo[CompIdT, IncMsgBuffT]): SndFN = ???
  def tbr(S: AgentTopo[CompIdT, IncMsgBuffT], D: AgentTopo[CompIdT, IncMsgBuffT]): RcvFN = (msg: TopoMsg) => ???

//  var IncMsgBuff: Iterable[IncMsgBuffT] = Iterable.empty

  // In case of Game Of Life:
  // val IncMsgBuff: Cell => IncMsgBuffT
  // where IncMsgBuffT is Tuple8[Int]

  def actionPerCell[V](cellId:Int, msgBuffer:IncMsgBuffT):V = ???
  def updateComponent[U](buffer:Map[Int,Vector[U]]):Unit = ???

//  def updateComponent[U](buffer:Map[Int,Vector[U]],perCellAct:(U,Iterable[U])=>U):Unit = ???



}
