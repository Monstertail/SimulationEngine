package example
package gameOfLifeMultiTileTopoMsgV0

import meta.runtime.Actor.AgentId
import meta.runtime.{Actor, Message}

class AgentTopo[T]{
  //To check, topo requires another type signature
  var Atopo:IndexedSeq[Actor]= _


  def tbs(Sid:T,Did:T) : TopoMsg = ???
  def processMessage[V](m:Message,c:AgentTopo[T]):V = ???
  def updateComponent[U](buffer:Iterable[(Int,Vector[U])],perCellAct:(U,Iterable[U])=>U):Unit = ???



}
