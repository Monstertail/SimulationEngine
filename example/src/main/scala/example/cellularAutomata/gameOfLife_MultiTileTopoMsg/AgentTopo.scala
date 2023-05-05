package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Actor.AgentId
import meta.runtime.{Actor, Message}

class AgentTopo[T]{
  //To check, topo requires another type signature
  var Atopo:IndexedSeq[Actor]= _
  var Ctopo:IndexedSeq[AgentTopo[T]]=_

//  def topo():IndexedSeq[AgentTopo[T]]= _

  def tbs(Sid:T,Did:T) : Message = ???



}
