package example
package gameOfLifeMultiTileTopoMsgV0

import meta.runtime.Message

import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.mutable.ListBuffer

class gridCoordinate(val x: Int, val y: Int){
  val r_index=x
  val c_index=y
}



class LocalArray2D[V: scala.reflect.ClassTag](w:Int,h:Int,gid:gridCoordinate) extends AgentTopo[gridCoordinate]{
//  val global_id:Pair = new Pair(0,0)
//  val width: Int = 0
//  val height:Int = 0
  val width=w
  val height=h
  val global_id =gid
  var currentBoard: Array[Array[V]] = Array.ofDim[V](h, w)
  //newBoard is used to store the updated states after step function for every cell
  var newBoard: Array[Array[V]] = Array.ofDim[V](h, w)



  def tbs(c1:LocalArray2D[V],c2:LocalArray2D[V]): TopoMsg = {
    (c1.global_id.c_index, c1.global_id.r_index, c2.global_id.c_index, c2.global_id.r_index) match {
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r+c2.height == c1r  =>
        RowMsgTop(c1.currentBoard(0))
      case (c1c, c1r, c2c, c2r) if c1c == c2c && c2r == c1r + c1.height =>
        RowMsgBottom(c1.currentBoard(c1.height - 1))
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c + c1.width == c2c =>
        ColMsgRight(c1.currentBoard.map(row => row(row.length - 1)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r && c1c  == c2c+c2.width =>
        ColMsgLeft(c1.currentBoard.map(row => row(0)).toVector)
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c  == c2c+c2.width =>
        DiagMsgTopLeft(c1.currentBoard(0)(0))
      case (c1c, c1r, c2c, c2r) if c1r == c2r + c2.height && c1c + c1.width == c2c =>
        DiagMsgTopRight(c1.currentBoard(0)(c1.width - 1))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c == c2c +c2.width=>
        DiagMsgBottomLeft(c1.currentBoard(c1.height - 1)(0))
      case (c1c, c1r, c2c, c2r) if c1r + c1.height == c2r && c1c + c1.width == c2c =>
        DiagMsgBottomRight(c1.currentBoard(c1.height - 1)(c1.width - 1))

    }

  }

  def processMessage(m:TopoMsg,c:LocalArray2D[V]):(Int,Vector[V]) ={
    m match {
      case DiagMsgBottomRight(v) => {
        (0,Vector(v.asInstanceOf[V]))
      }
      case DiagMsgBottomLeft(v) => {
        (1, Vector(v.asInstanceOf[V]))
      }
      case DiagMsgTopRight(v) => {
        (2, Vector(v.asInstanceOf[V]))
      }
      case DiagMsgTopLeft(v) => {
        (3, Vector(v.asInstanceOf[V]))
      }
      case RowMsgBottom(v) => {
        (4, v.toVector.asInstanceOf[Vector[V]])
      }
      case RowMsgTop(v) => {

        (5, v.toVector.asInstanceOf[Vector[V]])
      }
      case ColMsgRight(v) => {
        (6, v.toVector.asInstanceOf[Vector[V]])
      }

      case ColMsgLeft(v) => {
        (7, v.toVector.asInstanceOf[Vector[V]])
      }


    }


  }


  override def updateComponent[U](buffer:Iterable[(Int,Vector[U])], perCellAct:(U,Iterable[U])=>U):Unit = {

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