
package simulation.akka
package test

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class Tile2DArray[LST: ClassTag, MT, IncMsgBuffT](val cid: (Coordinate2D, Coordinate2D)) extends Component[LST, Coordinate2D, MT, IncMsgBuffT] {
  // For simplicity, assume only vertical partitioning (send an adjacent row). only rows are padded
  // a 2D array is uniquely defined by its shape (upper left, lower right)
  lazy val rows: Int = (cid._2.x - cid._1.x)
  lazy val cols: Int = (cid._2.y - cid._1.y)

  var oldBoard: Array[Array[LST]] = Array.ofDim[LST](rows, cols)
  var newBoard: Array[Array[LST]] = Array.ofDim[LST](rows, cols)

  //To store the messages from other components
//  var MsgBox: Array[Array[ArrayBuffer[IncMsgBuffT]]] = Array.ofDim[ArrayBuffer[IncMsgBuffT]](rows, cols)
  var MsgBox: Array[Array[ArrayBuffer[IncMsgBuffT]]] = Array.fill(rows, cols)(ArrayBuffer.empty[IncMsgBuffT])


  // Fill in the 2D grid with init values in the shape
  def fill(init: IndexedSeq[LST]): Unit = {
    var ctr: Int = 0
    for (i <- 0 to rows - 1) {
      for (j <- (0 to cols - 1)) {
        oldBoard(i)(j) = init(ctr)
        ctr += 1
      }
    }
  }


  // For simplicity, consider only top and bottom two directions.(Hard code the topo to decide messages to be sent)
  override def tbs(c: Component[LST, Coordinate2D, MT, IncMsgBuffT], preComp: Option[Iterable[LST] => Iterable[MT]]): () => ComponentMessage = {
    c match {
      case c: GeneralMessage[MT] =>
        c.cid match {
          case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
            // bottom
            if (x1 > cid._2.x) {
              if (preComp.isDefined) {
                val preComputeResult = preComp.get(oldBoard(rows - 1))
                () => new GeneralMessage[MT](preComputeResult, (Coordinate2D(cid._2.x, cid._1.y), cid._2))
              } else {
                () => new GeneralMessage[LST](oldBoard(rows - 1), (Coordinate2D(cid._2.x, cid._1.y), cid._2))
              }
              // top
            } else {
              if (preComp.isDefined) {
                val preComputeResult = preComp.get(oldBoard(1))
                () => new GeneralMessage[MT](preComputeResult, (cid._1, Coordinate2D(cid._1.x, cid._2.y)))
              } else {
                () => new GeneralMessage[LST](oldBoard(1), (cid._1, Coordinate2D(cid._1.x, cid._2.y)))
              }
            }
          case _ =>
            throw new Exception(f"Unsupported tbs direction in ${c}")
        }
      case _ =>
        throw new Exception(f"Unsupported tbs message type in ${c}")
    }
  }


//  override def tbr(c: Component[LST, (Coordinate2D, Coordinate2D), MT, IncMsgBuffT], accumulator: Option[Iterable[MT] => Iterable[IncMsgBuffT]]): RcvFN = (msg: ComponentMessage) => ???

  override def tbr(msg:ComponentMessage, accumulator: Option[Iterable[MT] => Iterable[IncMsgBuffT]]): Unit=
  {
    msg match {
      case x: GeneralMessage[MT] => {
        x.cid match {
          case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
            if (x1 > cid._2.x) {
              if (accumulator.isDefined) {
                val accResult = accumulator.get(x.content)

                accResult.zipWithIndex.foreach { case (elem, index) =>
                  MsgBox(rows-1)(index).append(elem)



                }

              } else {

                x.content.asInstanceOf[Iterable[IncMsgBuffT]].zipWithIndex.foreach { case (elem, index) =>
                  MsgBox(rows-1)(index).append(elem)
                }

              }

            } else {
              if (accumulator.isDefined) {
                val accResult = accumulator.get(x.content)

                accResult.zipWithIndex.foreach { case (elem, index) =>
                  MsgBox(0)(index).append(elem)
                }
                // TODO DEBUG
                println(MsgBox(rows-1)(0))

              } else {
                x.content.asInstanceOf[Iterable[IncMsgBuffT]].zipWithIndex.foreach { case (elem, index) =>
                  MsgBox(0)(index).append(elem)
                }
              }
            }
          case _ =>
            throw new Exception(" 2d array, unsupported messages!")
        }
      }
      case _ => throw new Exception("Unsupported messages!")
    }
  }

  // An example of pattern: accumulator. For other two patterns: inboxMsg and preCompute, they have the same interface.
  //The difference lies on the step1() and IncMsgBuffT.
  // In GoL,for pattern: inboxMsg,IncMsgBuffT is Boolean; while for pattern preCompute and accumulator, IncMsgBuffT is Int( to store the number of alive neighbors)
  class accumulator_APV[PRT] extends IncActionPerVertex[PRT]{

    var accumulator:PRT= _
    override def IncMSG(crd: Coordinate, crossComp: Iterable[IncMsgBuffT], initV:PRT,step1: Iterable[IncMsgBuffT] => PRT): PRT = {
      crossComp.isEmpty match {
        case false =>

          accumulator = step1(crossComp)
          accumulator

        case true =>

          accumulator = initV
          accumulator

      }


    }


    override def NoMoreMSG(crd: Coordinate, partial_result: PRT, step2: (PRT, Iterator[LST]) => PRT): PRT = {
      //traverse neighbor and got the partial result

      val gx = step2(partial_result, topo(crd.asInstanceOf[Coordinate2D]))
      gx

    }


  }


  class inboxMsg_APV[PRT <: Iterable[LST]] extends IncActionPerVertex[PRT] {

    // there is no need to IncMsg as we already store the cross-component messages in MsgBox with Iterable[LST] format.

    //PRT could be Iterable[LST]
    override def NoMoreMSG(crd: Coordinate, partial_result: PRT, step2: (PRT, Iterator[LST]) => PRT): PRT = {
      //traverse neighbor and got the partial result

      val gx = step2(partial_result, topo(crd.asInstanceOf[Coordinate2D]))
      gx

    }


  }


  //pattern preComp_APV[PRT] has the same interface with pattern accumulator_APV[PRT].
  // The difference is : for pattern preComp_APV[PRT], the value is pre-aggregated before sending messages,
  // while for pattern accumulator_APV[PRT], the value is accumulated after receiving messages.
  // Therefore, they have different tbr and tbs.
  class preComp_APV[PRT] extends IncActionPerVertex[PRT]{

    var partialRes: PRT = _

    override def IncMSG(crd: Coordinate, crossComp: Iterable[IncMsgBuffT], initV: PRT, step1: Iterable[IncMsgBuffT] => PRT): PRT = {
      crossComp.isEmpty match {
        case false =>

          partialRes = step1(crossComp)
          partialRes

        case true =>

          partialRes = initV
          partialRes

      }


    }


    override def NoMoreMSG(crd: Coordinate, partial_result: PRT, step2: (PRT, Iterator[LST]) => PRT): PRT = {
      //traverse neighbor and got the partial result

      val gx = step2(partial_result, topo(crd.asInstanceOf[Coordinate2D]))
      gx

    }

  }







}