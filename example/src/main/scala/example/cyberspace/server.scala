package example
package cyberspace

import squid.quasi.lift
import meta.classLifting.SpecialInstructions._
import meta.runtime.Actor.AgentId
import scala.collection.mutable.Map

@lift
class Server(var syncPeriod: Int, var batchMessages: Int) extends Actor {
    val content: Map[AgentId, String] = Map()
    var elapsed: Int = 0

    def get(): String = {
        // println("Server get mtd")
        content.toString()
    }

    def post(id: AgentId, newContent: String): Unit = {
        // println("Post to server")

        if (content.get(id).isEmpty) {
            content(id) = newContent
        } else {
            val old = content(id)
            content(id) = old + newContent
        }
    }

    def sync(c: Map[AgentId, String]): Unit = {
        // println("Server sync")
        c.foreach(x => {
            post(x._1, x._2)
        })
    }

    def main(): Unit = {
        while (true) {
            waitAndReply(1)
            elapsed = elapsed + 1
            if (elapsed >= syncPeriod){
                var batchCounter: Int = 1
                while (batchCounter < batchMessages) {
                    connectedAgents.foreach(s => {
                        asyncMessage(() => s.asInstanceOf[Server].sync(content))
                    })
                    batchCounter = batchCounter + 1
                }
                elapsed = 0
            }
        }
    }
}