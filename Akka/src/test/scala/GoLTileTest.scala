package simulation.akka
package test

import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class GoLTileTest extends FlatSpec {


    class GameOfLifeTileAgent(val tile: GameOfLifeTile) extends Actor {
        var msgGenerator: Map[Long, () => ComponentMessage] = Map[Long, ()=> ComponentMessage]()
//        var msgGenerator: Map[Long, SndFN] = Map[Long, SndFN]()

        override def run(): Int = {
            receivedMessages.foreach(i => {
//                tile.tbr(i.asInstanceOf[ComponentMessage])
                tile.tbr()(i.asInstanceOf[ComponentMessage],None)
                tile.tbr()(i.asInstanceOf[ComponentMessage],None)
            })
            receivedMessages.clear()
            connectedAgentIds.foreach(i => {
                sendMessage(i, msgGenerator(i)())
            })
            tile.update()
            1
        }
    }

    f"Create a number of tile agents for game of life example" should "run" in {
        val totalTiles: Int = 1
        
        val rowsPerTile: Int = 100
        val colsPerTile: Int = 1000
        val tileArrayRows:Int=1
        val tileArrayCols:Int=1

        //component topo

        val tiles = Range(0, totalTiles).map(i => {
            val x = new GameOfLifeTile((Coordinate2D(rowsPerTile*i, 0), Coordinate2D(rowsPerTile*(i+1)-1, colsPerTile)))
            x.fill(Range(0, rowsPerTile*colsPerTile).map(_ => Random.nextBoolean))
            x
        })

        //collection of actors(component(vertices))
        val agents: IndexedSeq[GameOfLifeTileAgent] = tiles.map(t => {
            new GameOfLifeTileAgent(t)
        })

        //update topo of actors(agents)
        if (totalTiles > 1) {
            Range(0, totalTiles).foreach(j => {
                val i = j.toLong
                agents(j).id = i
                j match {
                    case 0 => agents(j).connectedAgentIds = Vector(i+1)
                    case x if x ==totalTiles-1 => agents(j).connectedAgentIds = Vector(i-1)
                    case _ => agents(j).connectedAgentIds = Vector(i-1, i+1)
                }
            })
        }

        agents.foreach(a => {
            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.tile.tbs(tiles(i.toInt),None))).toMap
        })


        val snapshot1 = API.Simulate(agents, 200)
    }
}