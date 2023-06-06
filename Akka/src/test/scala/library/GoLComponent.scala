package simulation.akka
package test


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
class GameOfLifeTile(cid: (Coordinate2D, Coordinate2D)) extends Tile2DArray[Boolean, Int, Int](cid) {
  override def topo(c: Coordinate2D): Iterator[Boolean] = {
    for {
      i <- Iterator.range(-1, 1)
      j <- Iterator.range(-1, 1)
      if !(i == 0 && j == 0)
      dx = (c.x + i + rows) % rows
      dy = (c.y + j + cols) % cols
    }
    yield oldBoard(dx)(dy)
  }


  class GoLAPV_acc extends accumulator_APV[Int]{
    //a case of accumulator
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
    for (i <- (1 to rows - 1)) {
      for (j <- (0 to cols - 1)) {
        //                     newBoard(i)(j) = actionPerVertexFused(Coordinate2D(i, j))
        //                    newBoard(i)(j) = actionPerVertexStream(oldBoard(i)(j), topo(Coordinate2D(i, j)))

        val apv_acc = new GoLAPV_acc
        //three computing patterns have the same interface
        //for accumulator
        val partial_res = apv_acc.IncMSG(Coordinate2D(i, j), MsgBox(i)(j), buffer=>buffer.head )
        val alive_neighbor = apv_acc.NoMoreMSG(Coordinate2D(i, j), partial_res, (pr, intra_comp) => pr + intra_comp.map(_ == true).count)
        newBoard(i)(j)=apv_acc.step(oldBoard(i)(j),alive_neighbor)


      }
    }
    oldBoard = newBoard
  }

}
