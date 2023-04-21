package example
package gameOfLifeTile2DArrayOR

import scala.collection.mutable.{Map => MutMap}
//import lib.Graph.Torus2DGraph
import scala.util.Random
import scala.reflect.ClassTag
object MainInit {
  val liftedMain = meta.classLifting.liteLift {
    def apply(width: Int, height: Int): List[Actor] = {
      val totalComp: Int = 1
//      implicit val ct: ClassTag[Boolean] = scala.reflect.classTag[Boolean]
//      var currentBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)(ct)
//      var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)(ct)

//      val random = new Random()
//      for (i <- 0 until height; j <- 0 until width) {
//        currentBoard(i)(j) = random.nextBoolean()
//      }
      val array2D= new example.gameOfLifeTile2DArrayOR.Array2D(width, height)
      array2D.init()

      val tile = new Tile(width, height, array2D)

      val components = (1 to totalComp).map(x => tile).toList

      components
    }
  }
}

object Example extends App {

//  val cls2: ClassWithObject[Array2D] = Array2D.reflect(IR)
  val cls1: ClassWithObject[Tile] = Tile.reflect(IR)

  val mainClass = MainInit.liftedMain

  compileSims(List(cls1), Some(mainClass))
}
