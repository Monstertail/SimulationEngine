package example
package gameOfLifeMultiTileTopoMsg

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import scala.util.Random
import scala.reflect.ClassTag
import cloudcity.lib.Graph.GenerateGraph.Torus2DGraph
object MainInit {
  val liftedMain = meta.classLifting.liteLift {
    def apply(width: Int, height: Int, col:Int, row:Int): IndexedSeq[Actor] = {
      val totalComp: Int = row*col

      val tiles = Range(0, totalComp).map(i => {
        val L2DA = new example.gameOfLifeMultiTileTopoMsg.LocalArray2D[Boolean](width, height, new Pair((i / col)*height, (i % col)*width))
        val C2DA=new example.gameOfLifeMultiTileTopoMsg.Array2DCal(L2DA)
        val tile = new Tile(L2DA,C2DA)
        // Not strictly necessary. Just to be sure.
//        tile.array2D.topo(tile)
        tile.id = i
        tile
      })

      // 2D space
      val graph: Map[Long, Iterable[Long]] = Torus2DGraph(col, row)
      println(s"tile 0's neighbor: ${graph(0)}")
      tiles.zipWithIndex.map(c => {
//        c._1.connectedAgents = graph(c._2).map(i => tiles(i.toInt)).distinct
        c._1.LArray2D.Atopo = graph(c._2).map(i => tiles(i.toInt)).distinct
        //record topo for the neighbor components
        c._1.LArray2D.Ctopo=graph(c._2).map(i => tiles(i.toInt).LArray2D).distinct

      })



//      if (tiles(0).connectedAgents.size!=8){
//        println(s"ERROR!For tile ${tiles(0).id},TILE NEIGHBOR LENGTH SHOULD BE 8,not ${tiles(0).connectedAgents.size}!")
//      }



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
