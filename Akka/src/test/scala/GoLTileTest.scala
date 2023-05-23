package simulation.akka
package test

import cloudcity.lib.Graph.GenerateGraph.NonWrapping2DGraph
import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec

import scala.util.Random

class GoLTileTest extends FlatSpec {

    trait Coordinate
    case class Coordinate2D(x: Int, y: Int) extends Coordinate
    // case class TileCoordinate(x: Coordinate2D, y: Coordinate2D) extends Coordinate

    trait ComponentMessage extends Message
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

    // For simplicity, hard code Boolean type instead of taking a type variable
    class Boolean2DArray(val cid: (Coordinate2D, Coordinate2D)) extends Component[Boolean, Coordinate2D] {
        // For simplicity, assume only vertical partitioning (send an adjacent row). only rows are padded
        // a 2D array is uniquely defined by its shape (upper left, lower right)
        lazy val rows: Int = (cid._2.x - cid._1.x)+2
        lazy val cols: Int = (cid._2.y - cid._1.y)+2

        var oldBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)
        var newBoard: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)

        // Fill in the 2D grid with init values in the shape
        def fill(init: IndexedSeq[Boolean]): Unit = {
            var ctr: Int = 0
            for (i <- (1 to rows-2)) {
                for (j <- (1 to cols-2)) {
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
                                () => new Boolean2DArrayMessage(oldBoard(rows-2), cid)
                            // top
                            } else {
                                () => new Boolean2DArrayMessage(oldBoard(1), cid)
                            }
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x1 == cid._1.x && x2 == cid._2.x) =>
                            //right
                            if (y1 > cid._2.y) {
                                () => new Boolean2DArrayMessage(oldBoard.map(row => row(cols-2)), cid)
                                // left
                            } else {
                                () => new Boolean2DArrayMessage(oldBoard.map(row => row(1)), cid)
                            }
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x1 == cid._2.x+1 ) =>
                            //topleft
                            if (y1 == cid._2.y+1) {
                                () => new Boolean2DArrayMessage(Vector(oldBoard(1)(1)), cid)
                                // top right
                            } else {
                                () => new Boolean2DArrayMessage(Vector(oldBoard(1)(cols-2)), cid)
                            }

                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x2+1 == cid._1.x ) =>
                            //bottomleft
                            if (y1 == cid._2.y + 1) {
                                () => new Boolean2DArrayMessage(Vector(oldBoard(rows-2)(1)), cid)
                                // top right
                            } else {
                                () => new Boolean2DArrayMessage(Vector(oldBoard(rows-2)(cols - 2)), cid)
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
                            // from bottom to top
                            if (x1 > cid._2.x) {
                                x.content.copyToArray(oldBoard(0))
                                // from top to bottom
                            } else {
                                x.content.copyToArray(oldBoard(rows-1))
                            }
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x1 == cid._1.x && x2 == cid._2.x) =>
                            //from right to left
                            if (y1 > cid._2.y) {
                                for(i <- 1 to rows-2 ){
                                    oldBoard(i)(0)=x.content.toVector(i)
                                }

                                // from left to right
                            } else {
                                for (i <- 1 to rows - 2) {
                                    oldBoard(i)(cols-1) = x.content.toVector(i)
                                }
                            }
                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x1 == cid._2.x + 1) =>
                            //from topleft to bottom right
                            if (y1 == cid._2.y + 1) {
                                oldBoard(rows-1)(cols-1)=x.content.toVector(0)
                                // top right to bottom left
                            } else {
                                oldBoard(rows-1)(0)=x.content.toVector(0)
                            }

                        case (Coordinate2D(x1, y1), Coordinate2D(x2, y2)) if (x2 + 1 == cid._1.x) =>
                            //bottom left to top right
                            if (y1 == cid._2.y + 1) {
                                oldBoard(0)(cols-1)=x.content.toVector(0)
                                // bottom right to top left
                            } else {
                                oldBoard(0)(0)=x.content.toVector(0)
                            }

                        case _ =>
                            throw new Exception("Boolean 2d array, unsupported messages!")
                    }
                }
                case _ => throw new Exception("Unsupported messages!")
            }
        }

        def update(): Unit = {
            for (i <- (1 to rows-2)) {
                for (j <- (1 to cols-2)) {
                     newBoard(i)(j) = actionPerVertexFused(Coordinate2D(i, j))
//                    newBoard(i)(j) = actionPerVertexStream(oldBoard(i)(j), topo(Coordinate2D(i, j)))
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
        val colsPerTile: Int = System.getProperty("width", "1000").toInt
        val rowsPerTile: Int = System.getProperty("height", "1000").toInt
        val tilesPerRow: Int = System.getProperty("col", "1").toInt
        val tilesPerCol: Int = System.getProperty("row", "1").toInt
        val totalTurns: Int = System.getProperty("totalTurns", "200").toInt

        println(s"Width: $colsPerTile")
        println(s"Height: $rowsPerTile")
        println(s"Column: $tilesPerRow")
        println(s"Row: $tilesPerCol")
        println(s"Total turns: $totalTurns")

        
//        val rowsPerTile: Int = 1000
//        val colsPerTile: Int = 1000
//
//        val tilesPerRow:Int =1
//        val tilesPerCol:Int = 1
        val totalTiles: Int = tilesPerRow * tilesPerCol

        val tiles = Range(0, totalTiles).map(i => {
            val r_index=i/tilesPerRow
            val c_index=i% tilesPerRow
            val x = new GameOfLifeTile((Coordinate2D(rowsPerTile*r_index, colsPerTile*c_index), Coordinate2D(rowsPerTile*(r_index+1)-1, colsPerTile*(c_index+1)-1)))
            x.fill(Range(0, rowsPerTile*colsPerTile).map(_ => Random.nextBoolean))
            x
        })
        
        val agents: IndexedSeq[GameOfLifeTileAgent] = tiles.map(t => {
            new GameOfLifeTileAgent(t)
        })


        val graph: Map[Long, Iterable[Long]] = NonWrapping2DGraph(tilesPerRow, tilesPerCol)
        Range(0, totalTiles).foreach(j => {
            val i = j.toLong
            agents(j).id = i
            agents(j).connectedAgentIds=graph(i).toVector
        })


        agents.foreach(a => {
            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.tile.tbs(tiles(i.toInt)))).toMap
        })
        val snapshot1 = API.Simulate(agents, totalTurns)
    }
}