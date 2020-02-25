package meta.example.cas_example

import meta.deep.runtime.Actor
import squid.quasi.lift

import scala.collection.mutable.ListBuffer

@lift
class MainInit {
  def main(): List[Actor] = {
    val l = ListBuffer[Actor]()

//    val consensus_object: Consensus = new Consensus()
//
    (1 to 1).foreach(i => {
      val tx: Transaction = new Transaction()
      val cas: CAS = new CAS()
      val register: Register = new Register()
      tx.cas = cas
      tx.register = register
      l.append(tx, cas, register)
    })

//    l.append(consensus_object)
    l.toList

  }
}
