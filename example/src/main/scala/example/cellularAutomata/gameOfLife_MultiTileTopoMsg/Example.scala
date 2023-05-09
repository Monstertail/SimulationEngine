package example
package gameOfLifeMultiTileTopoMsg

import cloudcity.lib.Graph.GenerateGraph.NonWrapping2DGraph

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
        val L2DA = new example.gameOfLifeMultiTileTopoMsg.LocalArray2D[Boolean](width, height, new example.gameOfLifeMultiTileTopoMsg.gridCoordinate((i / col)*height, (i % col)*width))
//        val C2DA=new example.gameOfLifeMultiTileTopoMsg.Array2DCal(L2DA)
        val random = new Random()
        for (i <- 0 until height; j <- 0 until width) {
          L2DA.currentBoard(i)(j) = random.nextBoolean()
        }
        val tile = new Tile(L2DA)
        tile.id = i
        tile
      })

      // 2D space
      val graph: Map[Long, Iterable[Long]] = NonWrapping2DGraph(col, row)
      println(s"tile 0's neighbor: ${graph(0)}")
      tiles.zipWithIndex.map(c => {
        c._1.LArray2D.Atopo = graph(c._2).map(i => tiles(i.toInt)).toIndexedSeq


      })




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
