package simulation.akka
package test

import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec

import scala.util.Random

class GoLTileTest extends FlatSpec {

    trait Coordinate
    case class Coordinate2D(x: Int, y: Int) extends Coordinate
    // case class TileCoordinate(x: Coordinate2D, y: Coordinate2D) extends Coordinate

    trait ComponentMessage extends Message
    class GeneralMessage[MT](val content: Iterable[MT], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage
    class Boolean2DArrayMessage(val content: Iterable[Boolean], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage

    trait Component[T, C] {
        // def topo(c: C): Iterable[C] = ???
        def topo(c: C): Iterator[T] = ???
        // def actionPerVertex(v: T, neighbors: Iterable[T]): T = ???
        def actionPerVertexStream(v: T, vs: Iterator[T]): T = ???
        def actionPerVertexFused(v: C): T = ???
        def tbs(c: Component[T, C]): () => ComponentMessage = ???
        def tbr(msg: ComponentMessage): Unit = ???
    }

    //record the topo among tiles
    //In addition to making the code more graceful, what is the specific role of the nested class?
    class TilesTopo(tilesArrayRow: Int, tilesArrayCol: Int) extends Component[GameOfLifeTile,Coordinate2D]{

        lazy val tARow: Int = tilesArrayRow
        lazy val tACol: Int = tilesArrayCol
        var edgeList:Array[Array[GameOfLifeTile]]=Array.ofDim[GameOfLifeTile](tARow, tACol)


    }


    trait Monoid[A] {
        def op(a: A, b: A): A

        def unit: A
    }


    object Monoid {
        implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
            def op(a: Int, b: Int): Int = a + b

            def unit: Int = 0
        }

        implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
            def op(a: Boolean, b: Boolean): Boolean = a || b

            def unit: Boolean = false
//            def count(a: Int, b: Boolean): Int = a + (if (b) 1 else 0)
        }

        implicit val stringMonoid: Monoid[String] = new Monoid[String] {
            def op(a: String, b: String): String = a + b

            def unit: String = ""
        }


    }


    implicit class MonoidOps[A](a: A)(implicit ev: Monoid[A]) {
        def combine(b: A): A = ev.op(a, b)
        def empty: A = ev.unit
    }




    class actionPerVertex[MT]{
        def calculateDefault[MT](messages: Iterable[GeneralMessage[MT]])(implicit monoid: Monoid[MT]): MT = {
            val result= messages.map(x=>x.content.foldLeft(monoid.unit)(monoid.op)).foldLeft(monoid.unit)(monoid.op)
            result
        }

        def apply[LS](expression: Option[Iterable[GeneralMessage[MT]]=>LS], messages:Iterable[GeneralMessage[MT]] ):LS = {
            expression match {
                case None=>  calculateDefault(messages).asInstanceOf[LS]


                case Some(func) => func(messages)
            }
        }
    }



    //        def apA_accumM(messages:Iterable[ComponentMessage]):Int = {
    //            var sum: Int = 0
    //            for (message <- messages) {
    //                message match {
    //                    case IntMessage(value) =>
    //                        sum += value
    //                    case _ => // other types of message
    //                }
    //            }
    //            sum
    //
    //        }



    // For simplicity, hard code Boolean type instead of taking a type variable
    class Boolean2DArray(val cid: (Coordinate2D, Coordinate2D)) extends Component[Boolean, Coordinate2D] {
        // For simplicity, assume only vertical partitioning (send an adjacent row). only rows are padded
        // a 2D array is uniquely defined by its shape (upper left, lower right)
        lazy val rows: Int = (cid._2.x - cid._1.x)+2
        lazy val cols: Int = (cid._2.y - cid._1.y)

        var oldBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)
        var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)

        // Fill in the 2D grid with init values in the shape
        def fill(init: IndexedSeq[Boolean]): Unit = {
            var ctr: Int = 0
            for (i <- (0 to (cid._2.x - cid._1.x)-1)) {
                for (j <- (0 to cols-1)) {
                    oldBoard(i+1)(j) = init(ctr)
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

        def update(): Unit = {
            for (i <- (1 to rows-1)) {
                for (j <- (0 to cols-1)) {
                    // newBoard(i)(j) = actionPerVertexFused(Coordinate2D(i, j))
                    newBoard(i)(j) = actionPerVertexStream(oldBoard(i)(j), topo(Coordinate2D(i, j)))
                }
            }
            oldBoard = newBoard
        }
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
    }

    class GameOfLifeTileAgent(val tile: GameOfLifeTile) extends Actor {
        var msgGenerator: Map[Long, () => ComponentMessage] = Map[Long, ()=> ComponentMessage]()
        
        override def run(): Int = {
            receivedMessages.foreach(i => {
                tile.tbr(i.asInstanceOf[ComponentMessage])
            })
            receivedMessages.clear()
            connectedAgentIds.foreach(i => {
                sendMessage(i, msgGenerator(i)())
            })
            tile.update()
            1
        }
    }

    f"Create a number of tile agents for game of life example" should "run" in {
        val totalTiles: Int = 1
        
        val rowsPerTile: Int = 100
        val colsPerTile: Int = 1000
        val tileArrayRows:Int=1
        val tileArrayCols:Int=1
        val tiles=new TilesTopo(tileArrayRows,tileArrayCols)

//        val tiles = Range(0, totalTiles).map(i => {
//            val x = new GameOfLifeTile((Coordinate2D(rowsPerTile*i, 0), Coordinate2D(rowsPerTile*(i+1)-1, colsPerTile)))
//            x.fill(Range(0, rowsPerTile*colsPerTile).map(_ => Random.nextBoolean))
//            x
//        })

        for (i <- 0 until tiles.tARow; j <- 0 until tiles.tACol) {
            val tile = tiles.edgeList(i)(j)
            val x = new GameOfLifeTile((Coordinate2D(rowsPerTile * i, 0), Coordinate2D(rowsPerTile * (i + 1) - 1, colsPerTile)))
            x.fill(Range(0, rowsPerTile * colsPerTile).map(_ => Random.nextBoolean))

        }
        
//        val agents: IndexedSeq[GameOfLifeTileAgent] = tiles.map(t => {
//            new GameOfLifeTileAgent(t)
//        })
        val agents: IndexedSeq[GameOfLifeTileAgent] = for {
            i <- 0 until tiles.tARow
            j <- 0 until tiles.tACol
        } yield {
            val tile = tiles.edgeList(i)(j)
            new GameOfLifeTileAgent(tile)
        }

        if (totalTiles > 1) {
            Range(0, totalTiles).foreach(j => {
                val i = j.toLong
                agents(j).id = i
                j match {
                    case 0 => agents(j).connectedAgentIds = Vector(i+1)
                    case x if x ==totalTiles-1 => agents(j).connectedAgentIds = Vector(i-1)
                    case _ => agents(j).connectedAgentIds = Vector(i-1, i+1)
                }
            })
        }

//        agents.foreach(a => {
//            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.tile.tbs(tiles(i.toInt)))).toMap
//        })
        agents.foreach(a => {
            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.tile.tbs(tiles.edgeList(i.toInt / tiles.tACol) (i.toInt % tiles.tACol )))).toMap
        })


        val snapshot1 = API.Simulate(agents, 200)
    }
}