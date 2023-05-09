package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Actor.AgentId
import meta.runtime.{Actor, Message}

class AgentTopo[T]{
  //To check, topo requires another type signature
  var Atopo:IndexedSeq[Actor]= _
  //TO DO : Modify it to a map
//  var Ctopo:IndexedSeq[AgentTopo[T]]=_

//  def topo():IndexedSeq[AgentTopo[T]]= _

  def tbs(Sid:T,Did:T) : TopoMsg = ???
  def processMessage[V](m:Message,c:AgentTopo[T]):V = ???
  def updateComponent[V](buffer:Iterable[(Int,Vector[V])],perCellAct:(V,Iterable[V])=>V):Unit = ???

//  def updateMessage[V](c:AgentTopo[T] ,buffer:Iterable[V]):AgentTopo[T] = ???

//  def step(c:AgentTopo[T]):AgentTopo[T] = ???

}
