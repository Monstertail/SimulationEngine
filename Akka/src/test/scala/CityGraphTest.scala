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
//    class Boolean2DArrayMessage(val content: Iterable[Boolean], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage
    class GraphMessage[MT](val content: Iterable[(Coordinate2D,MT)]) extends ComponentMessage

    trait Component[T, C] {
        // def topo(c: C): Iterable[C] = ???
        def topo(c: C): Iterator[T] = ???
        // def actionPerVertex(v: T, neighbors: Iterable[T]): T = ???
        def actionPerVertexStream(v: T, vs: Iterator[T]): T = ???
        def actionPerVertexFused(v: C): T = ???
        def tbs(c: Component[T, C]): () => ComponentMessage = ???
        def tbr(msg: ComponentMessage): Unit = ???
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




    class actionPerVertex[LS,MT]{
        //LS->local state type; MT-> for every message, the data structure of content is Iterable[MT].
        def calculateDefault(messages: Iterable[GeneralMessage[MT]])(implicit monoid: Monoid[MT]): LS = {
            val result= messages.map(x=>x.content.foldLeft(monoid.unit)(monoid.op)).foldLeft(monoid.unit)(monoid.op)
            result.asInstanceOf[LS]
        }

        def apply(expression: Option[Iterable[GeneralMessage[MT]]=>LS], messages:Iterable[GeneralMessage[MT]] ):LS = {
            expression match {
                //default message processing: when there is no expression, sum all messages.
                case None=>
                     // provide an instance of Monoid[Int]
                    val monoidInstance: Monoid[Int] = Monoid.intMonoid
                    calculateDefault(messages)(monoidInstance.asInstanceOf[Monoid[MT]])


                case Some(func) => func(messages)
            }
        }
    }





    // For simplicity, hard code Boolean type instead of taking a type variable
    class IntGeneralGraph(val cid: (Coordinate2D, Coordinate2D)) extends Component[Int, Coordinate2D] {
        // For simplicity, assume only vertical partitioning (send an adjacent row). only rows are padded
        // a 2D array is uniquely defined by its shape (upper left, lower right)
        lazy val vertexNum: Int = (cid._2.y - cid._1.y)+1

        var edgeList: Map[Coordinate2D,Iterable[Coordinate2D]] = Map.empty
        var localState: Array[Int]=Array.ofDim(vertexNum)

        // Fill in each vertex inside this component with init values
        def fill(init: IndexedSeq[Int]): Unit = {
            var ctr: Int = 0
            for (i <- (0 to (vertexNum-1))) {
                localState(i) = init(ctr)
                ctr+=1
            }
        }

        def findKeyByDst(Dst: Coordinate2D, edgeList: Map[Coordinate2D, Iterable[Coordinate2D]]): Option[Coordinate2D] = {
            edgeList.find { case (_, neighbors) => neighbors.exists(_ == Dst) }.map { case (key, _) => key }
        }


        // For simplicity, consider only top and bottom two directions.
        override def tbs(c: Component[Int, Coordinate2D]): () => ComponentMessage = {
            c match {
                case c: IntGeneralGraph => {
                    var connected: Iterable[(Coordinate2D, Coordinate2D)] = Iterable.empty

                    for (i <- c.cid._1.y to c.cid._2.y) {
                        val neighbor_candidate = Coordinate2D(c.cid._1.x, i)
                        val keyOption = findKeyByDst(neighbor_candidate, edgeList)
                        keyOption.foreach { key =>
                            // Add the key and associated value to content
                            connected = connected ++ Iterable((key, neighbor_candidate))
                        }
                    }

                    if (connected.nonEmpty) {
                        // Generate messages based on non-empty content
                        // Example: return a function that creates a ComponentMessage

                        () => {
                            // Construct the message using the content
                            val messageContent = connected.map { case (src, dst) =>
                                (dst, localState(src.x-c.cid._1.x))
//                                content = content  ++ Iterable((dst, localState(src.x-c.cid._1.x)))
                            }

                            // Return the ComponentMessage
                            new GraphMessage[Int](messageContent)
                        }
                    } else {
                        throw new Exception(f"Unsupported direction in ${c}")
                    }
                }
                case _ =>
                    throw new Exception(f"Unsupported component type in ${c}")
            }
        }

        override def tbr(msg: ComponentMessage): Unit = {
            msg match {
                case x: GraphMessage[Int] => {
                    x.content match {
                        case (Coordinate2D(x1, y1), value) if (y1 == cid._1.y && y2 == cid._2.y) =>
                            if (x1 > cid._2.x) {
                                x.content.copyToArray(oldBoard(rows-1))
                            } else {
                                x.content.copyToArray(oldBoard(0))
                            }
                        case _ =>
                            throw new Exception("General graph, unsupported messages!")
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

        val tiles = Range(0, totalTiles).map(i => {
            val x = new GameOfLifeTile((Coordinate2D(rowsPerTile*i, 0), Coordinate2D(rowsPerTile*(i+1)-1, colsPerTile)))
            x.fill(Range(0, rowsPerTile*colsPerTile).map(_ => Random.nextBoolean))
            x
        })

        val agents: IndexedSeq[GameOfLifeTileAgent] = tiles.map(t => {
            new GameOfLifeTileAgent(t)
        })

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

        agents.foreach(a => {
            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.tile.tbs(tiles(i.toInt)))).toMap
        })




        val snapshot1 = API.Simulate(agents, 200)
    }
}