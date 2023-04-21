package example
package gameOfLifeTile2DArrayOR

import scala.util.Random
import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import squid.lib.transparencyPropagating

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

//The access and modification time complexity of ListBuffer is $O(n)$, while array is only O(1)
@lift
class Tile(val width: Int, val height: Int,val array2D: Array2D ) extends Actor {


    def main(): Unit = {
        while (true) {
            handleRPC()

            //iterate all cells
            array2D.update()
            //check the correctness
//            array2D.check()

            array2D.swap_ref()
            waitRounds(1)
        }
    }
}