package example
package gameOfLifeTile2DArrayOR

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import scala.util.Random
import scala.reflect.ClassTag
object MainInit {
  val liftedMain = meta.classLifting.liteLift {
    def apply(width: Int, height: Int): IndexedSeq[Actor] = {
      val totalComp: Int = 1

//      val array2D= new example.gameOfLifeTile2DArrayOR.Array2D(width, height)
//      array2D.init()
//
//      val tile = new Tile(width, height, array2D)
//
//      val components = (1 to totalComp).map(x => tile).toList
//
//      components

      val tiles = Range(0, totalComp).map(i => {
        val array2D = new example.gameOfLifeTile2DArrayOR.Array2D(width, height)
        array2D.init()
        val tile = new Tile(width, height, array2D)
        // Not strictly necessary. Just to be sure.
        tile.id = i
        tile
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
