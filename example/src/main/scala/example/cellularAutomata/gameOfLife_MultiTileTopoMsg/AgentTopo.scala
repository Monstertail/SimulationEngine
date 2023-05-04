package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Actor.AgentId
import meta.runtime.{Actor, Message}

class AgentTopo[T]{
  def topo(a:Actor):IndexedSeq[Actor]= _
  var  topoId :Iterable[AgentId] = List()

  class Msg{
    def tbs(encode:Int,Sid:T,Did:T): Message = {
      val msg = new Message()
      msg
    }
  }


}
