package simulation.akka
package test


import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compat.java8.collectionImpl.Accumulator
import scala.util.Random


    //------------------------Start of the library--------------------

    trait Coordinate
    case class Coordinate2D(x: Int, y: Int) extends Coordinate
    // case class TileCoordinate(x: Coordinate2D, y: Coordinate2D) extends Coordinate

    trait ComponentMessage extends Message

    //trait parameter requires scala 3
//    trait ComponentMessage[CompIdT](Sid:CompIdT, Did:CompIdT) extends Message
    class GeneralMessage[MT](val content: Iterable[MT], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage


    trait Component[LST, CompIdT, MT,IncMsgBuffT] {

        /* run at sim initialization time, not during a step
           generates a function to execute at runtime
        */
        type SndFN = () => ComponentMessage
        type RcvFN = ComponentMessage => Unit

        var compNeighborList: IndexedSeq[Component[LST,CompIdT,MT, IncMsgBuffT]] = _
        // record the component ID
        var CompId: (CompIdT,CompIdT) = _


        val sndHndlr: mutable.Map[(CompIdT,CompIdT), SndFN] = mutable.Map.empty

        val rcvHndlr: mutable.Map[(CompIdT,CompIdT), RcvFN] = mutable.Map.empty


//        // make send and rcv handlers ; call tbs and tbr
//        def init(): Unit = {
//            compNeighborList.foreach(n => {
//                //update sender handler
//                sndHndlr.update(n.CompId, tbs(n,None))
//                //update receive handler
//                rcvHndlr.update(n.CompId, tbr(n,None))
//            })
//        }

        def topo(c: CompIdT): Iterator[LST] = ???




        def tbs(c: Component[LST, CompIdT, MT, IncMsgBuffT],preComp:Option[Iterable[LST]=>Iterable[MT]]): () => ComponentMessage = ???

//        def tbr(c: Component[LST, CompIdT, MT, IncMsgBuffT],accumulator: Option[Iterable[MT]=>Iterable[IncMsgBuffT]]): RcvFN = (msg: ComponentMessage) => ???
        def tbr(msg: ComponentMessage,accumulator: Option[Iterable[MT]=>Iterable[IncMsgBuffT]]): Unit =  ???

        // Another way to decide the patterns
//        def tbr(c: Component[LST, CompIdT, IncMsgBuffT], tbrPattern: String): RcvFN = {
//            tbrPattern match {
//                case "inboxMsg" => (msg: ComponentMessage) => tbr_inboxMsg(msg)
//                case "accumulator" => (msg: ComponentMessage) => tbr_accumulator(msg)
//                case "preCompute" => (msg: ComponentMessage) => tbr_preCompute(msg)
//                case _ => throw new IllegalArgumentException("Invalid tbr pattern")
//            }
//        }

//        def tbr_inboxMsg(msg: ComponentMessage): Unit = ???
//
//        def tbr_accumulator(msg: ComponentMessage): Unit = ???
//
//        def tbr_preCompute(msg: ComponentMessage): Unit = ???


        trait IncActionPerVertex[PRT] {
            //update the result with information from cross-component messages

            //for pattern 1: Inbox message, IncMsgBuffT could be the same as Iterable[LST]
            //for pattern 2: Accumulator and pattern 3: pre-compute, we already process crossComp information
            // by calculating the partial results. So IncMsgBuffT could be the same as PRT(partial result type).
            def IncMSG(crd: Coordinate, crossComp: Iterable[IncMsgBuffT], initV:PRT,step1:Iterable[IncMsgBuffT] => PRT ): PRT = ???

            //update the result with information inside the component
            def NoMoreMSG(crd: Coordinate, partial_result: PRT,step2:(PRT,Iterator[LST]) => PRT): PRT = ???

        }





    }





    //------------------------End of the library--------------------







