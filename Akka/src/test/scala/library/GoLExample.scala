package simulation.akka
package test


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
class GameOfLifeTile(cid: (Coordinate2D, Coordinate2D)) extends Tile2DArray[Boolean, Boolean, Int](cid) {
  override def topo(c: Coordinate2D): Iterator[Boolean] = {
    for {
      i <- Iterator.range(-1, 1)
      j <- Iterator.range(-1, 1)
      if !(i == 0 && j == 0)
      dx = c.x + i
      dy = c.y + j
      if dx >= 0 && dx <= rows - 1 && dy >= 0 && dy <= cols - 1
    }
    yield oldBoard(dx)(dy)
  }


  //a case of pattern accumulator
  class GoLAPV_acc extends accumulator_APV[Int]{

    def step(oldState:Boolean, aliveNeighbors:Int):Boolean={

      if (oldState) {
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


  }

  def update(): Unit = {

    val apv_acc = new GoLAPV_acc
    for (i <- (0 to rows - 1)) {
      for (j <- (0 to cols - 1)) {

        //three computing patterns have the same interface
        //for accumulator

//        // TODO Debug: why MsgBox is always empty?
//        if (MsgBox(i)(j) != null && MsgBox(i)(j).isEmpty==false){
//          println(MsgBox(i)(j))
//        }

        val partial_res = apv_acc.IncMSG(Coordinate2D(i, j), MsgBox(i)(j),0, buffer=>buffer.head )
//        //Debug: hard code case
//        val partial_res = apv_acc.IncMSG(Coordinate2D(i, j),mutable.Iterable(1),0, buffer=>buffer.head )
        val alive_neighbor = apv_acc.NoMoreMSG(Coordinate2D(i, j), partial_res, (pr, intra_comp) => pr + intra_comp.count(_ == true))

        //step function
        newBoard(i)(j)=apv_acc.step(oldBoard(i)(j),alive_neighbor)
        //clean the msg buffer
        MsgBox(i)(j).clear()

      }
    }

    oldBoard = newBoard
  }

}
