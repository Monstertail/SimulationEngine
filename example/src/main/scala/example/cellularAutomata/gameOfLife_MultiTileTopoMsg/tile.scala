package example
package gameOfLifeMultiTileTopoMsg

import scala.util.Random
import meta.classLifting.SpecialInstructions._
import meta.runtime.Message
import squid.quasi.lift
import squid.lib.transparencyPropagating

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

//The access and modification time complexity of ListBuffer is $O(n)$, while array is only O(1)
@lift
class Tile(val LArray2D: LocalArray2D[Boolean],val C2DA: Array2DCal ) extends Actor {

//  @transparencyPropagating
//  def tell(state: Vector[Boolean], direction: Int, from: Int): Unit = {
//    // shall we set return type to Unit or Vector[boolean]?
//    array2D.decodeMsg(state, direction, from, id.toInt)
////    println("id"+id+"from:" + from)
//
//  }

    def main(): Unit = {
//      println("agent id " + id )
//      println(connectedAgents.map(_.id))

        while (true) {

            //send msgs
          // RPC
//            val msgs=array2D.packageMsg()
//            connectedAgents.map(x =>
//                x.asInstanceOf[Tile]).foreach(v => callAndForget(v.tell(msgs,1), 1))
//            var index = 0
//            connectedAgents.map(x => x.asInstanceOf[Tile]).foreach { v =>
//                callAndForget(v.tell(msgs(index), index,id.toInt), 1)
//                index =index+ 1
//            }

          //msg passing

            LArray2D.Atopo.foreach{v=> sendMessage(v.id,LArray2D.tbs(LArray2D,v.asInstanceOf[Tile].LArray2D)
            )}

            // Messages are sent and arrive at the beginning of the next round
            waitRounds(1)
            var m: Option[Message] = receiveMessage()


            while (m.isDefined) {
              C2DA.decodeMsg(m.get)
              m = receiveMessage()
            }
            //iterate all cells
          C2DA.update()
//            //check the correctness
//            array2D.check()

          C2DA.swap_ref()
//            waitRounds(1)
        }
    }
}