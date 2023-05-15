package example
package gameOfLifeMultiTileTopoMsgV0

import scala.util.Random
import meta.classLifting.SpecialInstructions._
import meta.runtime.Message
import squid.quasi.lift
import squid.lib.transparencyPropagating

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer


@lift
class Tile(val LArray2D: LocalArray2D[Boolean] ) extends Actor {

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

            //send the message
            LArray2D.Atopo.foreach { v =>
                sendMessage(v.id, LArray2D.tbs(LArray2D, v.asInstanceOf[Tile].LArray2D)
                )
            }

            waitRounds(1)
            //receive the message
            var m = receiveMessage()
            var messages = ListBuffer[example.gameOfLifeMultiTileTopoMsg.TopoMsg]()

            while (m.isDefined) {
                messages += m.get.asInstanceOf[example.gameOfLifeMultiTileTopoMsg.TopoMsg]
                m = receiveMessage()
            }

            //    //debug
            //    println(s"tile id: $id :"+"Message size=" + messages.size)

            LArray2D.updateComponent(messages
              .map(m => {
                  val (i, v) = LArray2D.processMessage(m, LArray2D)
                  (i, v)
              }),
                (current: Boolean, neighbors: Iterable[Boolean]) => {
                    val aliveNeighbors = neighbors.count(_ == true)
                    if (current) {
                        if (aliveNeighbors == 2 || aliveNeighbors == 3) {
                            true
                        } else {
                            false
                        }
                    } else {
                        if (aliveNeighbors == 3) {
                            true
                        } else {
                            false
                        }
                    }
                }
            )


        }
    }
}