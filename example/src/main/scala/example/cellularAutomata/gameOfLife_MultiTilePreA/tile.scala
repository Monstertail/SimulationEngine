package example
package gameOfLifeMultiTilePreA

import scala.util.Random
import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import squid.lib.transparencyPropagating

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

//The access and modification time complexity of ListBuffer is $O(n)$, while array is only O(1)
@lift
class Tile(val width: Int, val height: Int,val array2D: Array2D ) extends Actor {

  @transparencyPropagating
  def tell(state: Vector[Boolean], direction: Int, from: Int): Unit = {
    // shall we set return type to Unit or Vector[boolean]?
    array2D.decodeMsg(state, direction, from, id.toInt)
//    println("id"+id+"from:" + from)

  }

    def main(): Unit = {
//      println("agent id " + id )
//      println(connectedAgents.map(_.id))

        while (true) {
            handleRPC()
            //send msgs
            val msgs=array2D.packageMsg()
//            connectedAgents.map(x =>
//                x.asInstanceOf[Tile]).foreach(v => callAndForget(v.tell(msgs,1), 1))
            var index = 0
            connectedAgents.map(x => x.asInstanceOf[Tile]).foreach { v =>
                callAndForget(v.tell(msgs(index), index,id.toInt), 1)
                index =index+ 1
            }
// check if the array changed due to RPC
//            array2D.check()
            waitAndReply(1)
//            //debug: check the correctness of current Board after RPC
//            array2D.check()
            //iterate all cells
            array2D.update()


            array2D.swap_ref()
//            waitRounds(1)
        }
    }
}