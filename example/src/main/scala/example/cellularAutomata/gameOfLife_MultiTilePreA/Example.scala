package example
package gameOfLifeMultiTilePreA

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import scala.util.Random
import scala.reflect.ClassTag
import cloudcity.lib.Graph.GenerateGraph.Torus2DGraph
object MainInit {
  val liftedMain = meta.classLifting.liteLift {
    def apply(width: Int, height: Int, row:Int, col:Int): IndexedSeq[Actor] = {
      val totalComp: Int = row*col

      val tiles = Range(0, totalComp).map(i => {
        val array2D = new example.gameOfLifeMultiTilePreA.Array2D(width, height)
        array2D.init(i)
        val tile = new Tile(width, height, array2D)
        // Not strictly necessary. Just to be sure.
        tile.id = i
        tile
      })

      // 2D space
      val graph: Map[Long, Iterable[Long]] = Torus2DGraph(col, row)
      println(s"tile 0's neighbor: ${graph(0)}")
      tiles.zipWithIndex.map(c => {
        c._1.connectedAgents = graph(c._2).map(i => tiles(i.toInt))
      })

      if (tiles(0).connectedAgents.size!=8){
        println(s"ERROR!For tile ${tiles(0).id},TILE NEIGHBOR LENGTH SHOULD BE 8,not ${tiles(0).connectedAgents.size}!")
      }



      tiles
    }
  }
}

object Example extends App {

//  val cls2: ClassWithObject[Array2D] = Array2D.reflect(IR)
  val cls1: ClassWithObject[Tile] = Tile.reflect(IR)

  val mainClass = MainInit.liftedMain

  compileSims(List(cls1), Some(mainClass))
}
