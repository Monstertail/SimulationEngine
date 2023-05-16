package example
package gameOfLifeMultiTileTopoMsg

import meta.runtime.Message

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
class gridCoordinate(val x: Int, val y: Int){
  val r_index=x
  val c_index=y
}



class LocalArray2D[V: scala.reflect.ClassTag](w:Int,h:Int,gid:gridCoordinate) extends AgentTopo[gridCoordinate,Iterable[V]]{

  val width=w
  val height=h

  CompId=gid
  var currentBoard: Array[Array[V]] = Array.ofDim[V](h, w)
//  //newBoard is used to store the updated states after step function for every cell
  var newBoard: Array[Array[V]] = Array.ofDim[V](h, w)

//  IncMsgBuff=Array.ofDim[Iterable[V]](h*w)
    val recvMsgBuff: mutable.Map[Int, Vector[V]] = mutable.Map.empty


  override def init(): Unit = {
    compNeighborList.foreach(n => {
      val comp=n.asInstanceOf[LocalArray2D[V]]
      //update sender handler
      sndHndlr.update(comp.CompId, tbs(this, comp))
      //update receive handler
      rcvHndlr.update(comp.CompId, tbr(comp, this))
    })
  }

  def tbs(c1: LocalArray2D[V], c2: LocalArray2D[V]): SndFN = {
    (c1.CompId.c_index, c1.CompId.r_index, c2.CompId.c_index, c2.CompId.r_index) match {
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r + c2.height == c1r =>
        ()=> VectorMsg(c1.CompId,c1.currentBoard(0))
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r == c1r + c1.height =>
        ()=>VectorMsg(c1.CompId,c1.currentBoard(c1.height - 1))
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c + c1.width == c2c =>
        ()=>VectorMsg(c1.CompId,c1.currentBoard.map(row => row(row.length - 1)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c == c2c + c2.width =>
        ()=>VectorMsg(c1.CompId,c1.currentBoard.map(row => row(0)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c == c2c + c2.width =>
        ()=>VectorMsg(c1.CompId,Vector(c1.currentBoard(0)(0)))
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c + c1.width == c2c =>
        ()=>VectorMsg(c1.CompId,Vector(c1.currentBoard(0)(c1.width - 1)))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c == c2c + c2.width =>
        ()=>VectorMsg(c1.CompId,Vector(c1.currentBoard(c1.height - 1)(0)))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c + c1.width == c2c =>
        ()=>VectorMsg(c1.CompId,Vector(c1.currentBoard(c1.height - 1)(c1.width - 1)))

    }

  }

  def tbr(c1: LocalArray2D[V], c2: LocalArray2D[V]): RcvFN = (msg: TopoMsg) => {
    (c1.CompId.c_index, c1.CompId.r_index, c2.CompId.c_index, c2.CompId.r_index) match {
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r + c2.height == c1r =>
        //from top row in c1 to bottom row in c2
        recvMsgBuff += ( 5 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)

      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r == c1r + c1.height =>
        //from bottom row in c1 to the top row in c2
        recvMsgBuff += ( 4 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c + c1.width == c2c =>
        //from the right column in c1 to left column in c2
        recvMsgBuff += ( 6 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c == c2c + c2.width =>
        //from the left column in c1 to right column in c2
        recvMsgBuff += ( 7 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c == c2c + c2.width =>
        //from the top left in c1 to bottom right in c2
        recvMsgBuff += ( 3 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c + c1.width == c2c =>
        //from the top right in c1 to bottom left in c2
        recvMsgBuff += ( 2 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c == c2c + c2.width =>
        //from the bottom left in c1 to top right in c2
        recvMsgBuff += ( 1 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c + c1.width == c2c =>
        //from the bottom right in c1 to top left in c2
        recvMsgBuff += ( 0 -> msg.asInstanceOf[VectorMsg[gridCoordinate,V]].v.toVector)
    }
  }

  def applyMessage(msg:TopoMsg):Unit={
        msg match {
          case VectorMsg(sid, v) => {
            rcvHndlr(sid.asInstanceOf[gridCoordinate])(msg)
          }
        }
  }


 def updateComponent[U](buffer:Map[Int,Vector[U]], perCellAct:(U,Iterable[U])=>U):Unit = {

    val receiveMessage=buffer

    for (i<-0 until  height) {

      for (j<-0 until  width) {
//      //--------------------------------debug performance by lamda function: 1600 ms
//      val neighbors = (-1 to 1).flatMap { ni =>
//        (-1 to 1).flatMap { nj =>
//          if (ni == 0 && nj == 0) {
//            None
//          } else {
//            val r = i + ni
//            val c = j + nj
//            if (r >= 0 && r < width && c >= 0 && c < height) {
//              Some(currentBoard(r)(c).asInstanceOf[U])
//            } else {
//              None
//            }
//          }
//        }
//      }

        val  neighbors= ListBuffer[U]()
        for (ni<- -1 to  1) {

          for (nj <- -1 to 1) {
            if (!(ni == 0 && nj == 0)) {
              //check the location of neighbors and update
              //row index of the neighbor
              val r = i + ni
              //column index of the neighbor
              val c = j + nj
//              //debug for performance:163 ms
//              if (r >= 0 && r < width && c >= 0 && c < height){
//                neighbors+= currentBoard(r)(c).asInstanceOf[U]
//              }

              // look up the state of the neighbors: 190 ms
              if (r == -1) {
                if (c == -1) {
                  //topLeft

                  receiveMessage.find { case (i, v) => i == 0 }.foreach { case (i, v) =>
                    neighbors += v(0)
                  }
                }
                else if (c == width) {
                  //topRight

                  receiveMessage.find { case (i, v) => i == 1 }.foreach { case (i, v) =>
                    neighbors += v(0)
                  }
                }

                else {
                  //topEdge(c)
                  receiveMessage.find { case (i, v) => i == 4 }.foreach { case (i, v) =>
                    neighbors += v(c)
                  }

                }
              } else if (r == height) {
                if (c == -1) {
                  // bottomLeft
                  receiveMessage.find { case (i, v) => i == 2 }.foreach { case (i, v) =>
                    neighbors += v(0)
                  }

                }
                else if (c == width) {
                  //bottomRight

                  receiveMessage.find { case (i, v) => i == 3 }.foreach { case (i, v) =>
                    neighbors += v(0)
                  }
                }
                else {
                  //bottomEdge(c)

                  receiveMessage.find { case (i, v) => i == 5 }.foreach { case (i, v) =>
                    neighbors += v(c)
                  }
                }
              } else {
                if (c == -1) {
                  //leftEdge(r)
                  receiveMessage.find { case (i, v) => i == 6 }.foreach { case (i, v) =>
                    neighbors += v(r)
                  }
                } else if (c == width) {
                  //rightEdge(r)
                  receiveMessage.find { case (i, v) => i == 7 }.foreach { case (i, v) =>
                    neighbors += v(r)
                  }
                } else {
                  neighbors+= currentBoard(r)(c).asInstanceOf[U]
                }
              }

            }

          }

        }



        // step function:apply the game of life rules to determine the next state

        newBoard(i)(j)=perCellAct(currentBoard(i)(j).asInstanceOf[U],neighbors).asInstanceOf[V]

      }

    }

   //clean the map
    buffer.clear()

    // swap the reference

    val temp = currentBoard
    // check if two references point to the same object in memory
    // If isSameArray is false, then a new array was created and the pointers were not simply swapped.
    val isSameArray = currentBoard eq temp

    currentBoard = newBoard
    val CNSameArray = currentBoard eq newBoard

    newBoard = temp
    val NTSameArray = temp eq newBoard

    if (!(isSameArray && CNSameArray && NTSameArray)) {
      println(s"ERROR:two references point to the different objects in memory!")
    }

  }








}