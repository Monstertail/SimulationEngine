package simulation.akka
package test


import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


    //------------------------Start of the library--------------------

    trait Coordinate
    case class Coordinate2D(x: Int, y: Int) extends Coordinate
    // case class TileCoordinate(x: Coordinate2D, y: Coordinate2D) extends Coordinate



    trait ComponentMessage extends Message
//    class GeneralMessage[MT](val content: Iterable[MT], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage
    class Boolean2DArrayMessage(val content: Iterable[Boolean], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage

    trait Component[T, C] {

        /* run at sim initialization time, not during a step
           generates a function to execute at runtime
        */
        type SndFN = () => ComponentMessage
        type RcvFN = ComponentMessage => Unit

        def topo(c: C): Iterator[T] = ???
        // (not belong to this library)handwritten version before
        def actionPerVertexStream(v: T, vs: Iterator[T]): T = ???
        def actionPerVertexFused(v: C): T = ???


        def tbs(c: Component[T, C]): () => ComponentMessage = ???

        //(not belong to this library) handwritten version with pre-allocating
        def tbr(msg: ComponentMessage): Unit = ???

        //staging three computing patterns
        def tbr_inboxMsg(msg: ComponentMessage): Unit = ???
        def tbr_accumulator(msg: ComponentMessage): Unit = ???
        def tbr_preCompute(msg: ComponentMessage): Unit = ???


        class IncActionPerVertex[MT, LS] {
            //update the result with information from cross-component messages

            //for pattern 1: Inbox message, we stack the messages for every vertex by tbr_inboxMsg()
            //for pattern 2: Accumulator and pattern 3: pre-compute, we already process crossComp information
            // by calculating the partial results. So there is no need to store the messages.
            def IncMSG(crd: Coordinate, crossComp: Option[Iterable[MT]]): Unit = ???

            //update the result with information inside the component

            //for pattern 1: Inbox message, partial_result is none
            //for pattern 2: Accumulator and pattern 3: pre-compute, partial_result is already calculated
            def NoMoreMSG[PR](crd: Coordinate, partial_result: Option[PR]): LS = ???

        }


    }


    //------------------------End of the library--------------------




    // For simplicity, hard code Boolean type instead of taking a type variable
    class Boolean2DArray(val cid: (Coordinate2D, Coordinate2D)) extends Component[Boolean, Coordinate2D] {
        // For simplicity, assume only vertical partitioning (send an adjacent row). only rows are padded
        // a 2D array is uniquely defined by its shape (upper left, lower right)
        lazy val rows: Int = (cid._2.x - cid._1.x)+2
        lazy val cols: Int = (cid._2.y - cid._1.y)

        var oldBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)
        var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)

        //To store the messages from other components
        var MsgBox: Array[Array[ArrayBuffer[Boolean]]]=Array.ofDim[ArrayBuffer[Boolean]](rows-2, cols)
        var IncUpdateResults: Array[Array[Int]]=Array.ofDim[Int](rows-2, cols)


        // Fill in the 2D grid with init values in the shape
        def fill(init: IndexedSeq[Boolean]): Unit = {
            var ctr: Int = 0
            for (i <- (0 to (cid._2.x - cid._1.x)-1)) {
                for (j <- (0 to cols-1)) {
                    oldBoard(i+1)(j) = init(ctr)
                    IncUpdateResults(i)(j)=0
                    ctr+=1
                }
            }
        }

        // For simplicity, consider only top and bottom two directions.
        override def tbs(c: Component[Boolean, Coordinate2D]): () => ComponentMessage = {
            c match {
                case c: Boolean2DArray => {
                    c.cid match { 
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
                            // bottom
                            if (x1 > cid._2.x) {
                                () => new Boolean2DArrayMessage(oldBoard(rows-1), (Coordinate2D(cid._2.x, cid._1.y), cid._2))
                            // top
                            } else {
                                () => new Boolean2DArrayMessage(oldBoard(1), (cid._1, Coordinate2D(cid._1.x, cid._2.y)))
                            }
                        case _ =>
                            throw new Exception(f"Unsupported tbs direction in ${c}")
                            () => new Boolean2DArrayMessage(oldBoard.flatten.toVector, cid)
                    }
                }
                case _ =>
                    () => new Boolean2DArrayMessage(oldBoard.flatten.toVector, cid)
            }
        }

        override def tbr(msg: ComponentMessage): Unit = {
            msg match {
                case x: Boolean2DArrayMessage => {
                    x.cid match {
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>                         
                            if (x1 > cid._2.x) {
                                x.content.copyToArray(oldBoard(rows-1))
                            } else {
                                x.content.copyToArray(oldBoard(0))
                            }
                        case _ =>
                            throw new Exception("Boolean 2d array, unsupported messages!")
                    }
                }
                case _ => throw new Exception("Unsupported messages!")
            }
        }

        override def tbr_accumulator(msg: ComponentMessage): Unit = {

            msg match {
                case x: Boolean2DArrayMessage => {
                    x.cid match {
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
                            if (x1 > cid._2.x) {
                                for (i <- 0 to cols - 1) {
                                    //partially update the result
                                    x.content.foreach { m =>
                                        if (i - 1 >= 0) {
                                            if (m) {
                                                IncUpdateResults(rows-3)(i-1)+=1
                                            }
                                        }
                                        if (m) {
                                            IncUpdateResults(rows - 3)(i) += 1
                                        }
                                        if (i + 1 <= cols - 1) {
                                            if (m) {
                                                IncUpdateResults(rows - 3)(i+1) += 1
                                            }
                                        }
                                    }


                                }

                            } else {
                                for (i <- 0 to cols - 1) {
                                    //partially update the result
                                    x.content.foreach { m =>
                                        if (i - 1 >= 0) {
                                            if (m) {
                                                IncUpdateResults(0)(i - 1) += 1
                                            }
                                        }
                                        if (m) {
                                            IncUpdateResults(0)(i) += 1
                                        }
                                        if (i + 1 <= cols - 1) {
                                            if (m) {
                                                IncUpdateResults(0)(i + 1) += 1
                                            }
                                        }
                                    }


                                }
                            }
                        case _ =>
                            throw new Exception("Boolean 2d array, unsupported messages!")
                    }
                }
                case _ => throw new Exception("Unsupported messages!")
            }



        }

        override def tbr_inboxMsg(msg: ComponentMessage): Unit = {

            msg match {
                case x: Boolean2DArrayMessage => {
                    x.cid match {
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
                            if (x1 > cid._2.x) {
                                for(i <- 0 to cols-1){
                                    //copy the content from messages to all vertices
                                    x.content.foreach{m=>
                                      if (i-1>=0) {MsgBox(rows-3)(i-1)+=m}
                                        MsgBox(rows-3)(i) += m
                                      if (i+1<=cols-1){MsgBox(rows-3)(i+1) += m}
                                    }


                                }

                            } else {
                                for (i <- 0 to cols - 1) {
                                    //copy the content from messages to all vertices
                                    x.content.foreach { m =>
                                        if (i - 1 >= 0) {
                                            MsgBox(0)(i - 1) += m
                                        }
                                        MsgBox(0)(i) += m
                                        if (i + 1 <= cols - 1) {
                                            MsgBox(0)(i + 1) += m
                                        }
                                    }


                                }
                            }
                        case _ =>
                            throw new Exception("Boolean 2d array, unsupported messages!")
                    }
                }
                case _ => throw new Exception("Unsupported messages!")
            }

        }

////        //In GOL, there is a preComputing case
//        override def tbr_preCompute(msg: ComponentMessage): Unit = {
//            msg match {
//                case x: GeneralMessage[Int] => {
//                    x.cid match {
//                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (y1 == cid._1.y && y2 == cid._2.y) =>
//                            if (x1 > cid._2.x) {
//                                x.content.asInstanceOf[Iterable[Int]].copyToArray(IncUpdateResults(rows-3))
//                            } else {
//                                x.content.asInstanceOf[Iterable[Int]].copyToArray(IncUpdateResults(0))
//                            }
//                        case _ =>
//                            throw new Exception("Boolean 2d array, unsupported messages!")
//                    }
//                }
//                case _ => throw new Exception("Unsupported messages!")
//            }
//        }


//        def update(): Unit = {
//            for (i <- (1 to rows-1)) {
//                for (j <- (0 to cols-1)) {
////                     newBoard(i)(j) = actionPerVertexFused(Coordinate2D(i, j))
////                    newBoard(i)(j) = actionPerVertexStream(oldBoard(i)(j), topo(Coordinate2D(i, j)))
//
//                    val apv=new IncActionPerVertex[Boolean,Boolean]
//                    //three computing patterns have the same interface
//                    apv.IncMSG(Coordinate2D(i, j),Option(MsgBox(i)(j)))
//                    newBoard(i)(j)=apv.NoMoreMSG(Coordinate2D(i, j),Option(IncUpdateResults(i)(j)))
//
//
//                }
//            }
//            oldBoard = newBoard
//        }
    }

    class GameOfLifeTile(cid: (Coordinate2D, Coordinate2D)) extends Boolean2DArray(cid) {
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

        override def actionPerVertexStream(v: Boolean, vs: Iterator[Boolean]): Boolean = {
            var aliveNeighbors: Int = 0
            while (vs.hasNext){
                // val (x, y) = vs.next
                // if (oldBoard(x)(y)) aliveNeighbors += 1
                if (vs.next()) {
                    aliveNeighbors += 1
                }
            }
            
            if (v) {
                if (aliveNeighbors > 3 || aliveNeighbors < 1) {
                    false
                } else {
                    true
                }
            } else {
                if (aliveNeighbors == 3) {
                    true
                } else {
                    false
                }
            }
        }

        override def actionPerVertexFused(c: Coordinate2D): Boolean = {
            // topo(Coordinate2D(i, j)).map(k => oldBoard(k.x)(k.y))
            var i: Int = c.x
            var j: Int = c.y
            var ni: Int = -1
            var nj: Int = -1

            var aliveNeighbors: Int = 0
            while (ni <= 1) {
                nj = -1
                while (nj <= 1) {
                    if (!(ni == 0 && nj == 0)) {
                        // val row = i + cols  // handle wrapping at edges
                        // val col = j + rows  // handle wrapping at edges
                        val row = (i + ni + rows) % rows // handle wrapping at edges
                        val col = (j + nj + cols) % cols // handle wrapping at edges
                        if (oldBoard(row)(col)) 
                            aliveNeighbors = aliveNeighbors + 1
                    }
                    nj = nj + 1
                }
                ni = ni + 1
            }

            if (oldBoard(i)(j)) {
                if (aliveNeighbors > 3 || aliveNeighbors < 1) {
                    false
                } else {
                    true
                }
            } else {
                if (aliveNeighbors == 3) {
                    true
                } else {
                    false
                }
            }
        }


        class GoLAPV extends IncActionPerVertex[Boolean, Boolean] {
            var accumulator: Int = 0

            override def IncMSG(crd: Coordinate, crossComp: Option[Iterable[Boolean]]): Unit = {
                crossComp match {
                    case Some(iterable) =>
                        //for Pattern 1: inbox messages
                        accumulator = iterable.count(_ == true)

                    case None =>
                    //for pattern 2: Accumulator and pattern 3: pre-compute, we calculate the partial results instead of storing the messages
                    // We can skip IncMSG in this case, only need to set accumulator to 0
                        accumulator=0

                }
            }


            def NoMoreMSG( crd:Coordinate,pr: Option[Int]): Boolean = {

                var ni = -1
                var nj = -1
                val i=crd.asInstanceOf[Coordinate2D].x
                val j=crd.asInstanceOf[Coordinate2D].y
                //for pattern 1 Inbox message, pr is none
                //for pattern 2: Accumulator and pattern 3: pre-compute, pr is already calculated
                pr match {
                    case Some(value) => accumulator = accumulator + value
                    case None =>
                }

                while (ni <= 1) {
                    nj = -1
                    while (nj <= 1) {
                        if (!(ni == 0 && nj == 0)) {
                            val row = i + ni // handle wrapping at edges
                            val col = j + nj // handle wrapping at edges

                            if(row>=0 && row <=rows-3 && col>=0 && col<=cols-1){
                                if (oldBoard(row)(col))
                                    accumulator = accumulator + 1

                            }


                        }
                        nj = nj + 1
                    }
                    ni = ni + 1
                }

                if (oldBoard(i)(j)) {
                    if (accumulator > 3 || accumulator < 1) {
                        false
                    } else {
                        true
                    }
                } else {
                    if (accumulator == 3) {
                        true
                    } else {
                        false
                    }
                }




            }

        }

        def update(): Unit = {
            for (i <- (1 to rows - 3)) {
                for (j <- (0 to cols - 1)) {
                    //                     newBoard(i)(j) = actionPerVertexFused(Coordinate2D(i, j))
                    //                    newBoard(i)(j) = actionPerVertexStream(oldBoard(i)(j), topo(Coordinate2D(i, j)))

                    val apv = new GoLAPV
                    //three computing patterns have the same interface
                    apv.IncMSG(Coordinate2D(i, j), Option(MsgBox(i)(j)))
                    newBoard(i)(j) = apv.NoMoreMSG(Coordinate2D(i, j), Option(IncUpdateResults(i)(j)))


                }
            }
            oldBoard = newBoard
        }

    }




