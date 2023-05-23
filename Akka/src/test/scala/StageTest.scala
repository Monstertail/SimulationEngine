package simulation.akka
package test

import meta.runtime._
import simulation.akka.API._
import org.scalatest.FlatSpec
import scala.util.Random

class StageTest extends FlatSpec {

    trait Coordinate
    case class Coordinate2D(x: Int, y: Int) extends Coordinate
    // case class TileCoordinate(x: Coordinate2D, y: Coordinate2D) extends Coordinate

    trait ComponentMessage extends Message
//    class Boolean2DArrayMessage(val content: Iterable[Boolean], val cid: (Coordinate2D, Coordinate2D)) extends ComponentMessage
    class GeneralMessage[MsgT](val content: Iterable[MsgT], val cid: Coordinate2D) extends ComponentMessage


    trait Component[T, C] {
        // def topo(c: C): Iterable[C] = ???
        def topo(c: C): Iterator[T] = ???
        // def actionPerVertex(v: T, neighbors: Iterable[T]): T = ???
        def actionPerVertexStream(v: T, vs: Iterator[T]): T = ???
        def actionPerVertexFused(v: C): T = ???
        def tbs(c: Component[T, C]): () => ComponentMessage = ???
        def tbs_stage(c: Component[T, C]): () => ComponentMessage = ???
        def tbr(msg: ComponentMessage): Unit = ???
    }

    // For simplicity, hard code Boolean type instead of taking a type variable
    class ComponentTopo(val cid: Coordinate2D) extends Component[Int, Coordinate2D] {

        lazy val cols: Int = cid.y
        var comp: Array[Int] = Array.ofDim[Int]( cols)



        // Fill in the 2D grid with init values in the shape
        def fill(): Unit = {

            for (i <- (0 to cols-1)) {

                comp(i) = 1


            }
        }

        // For simplicity, consider only one row of messages
        override def tbs(c: Component[Int, Coordinate2D]): () => ComponentMessage = {
            c match {
                case c: ComponentTopo=> {
                    c.cid match { 
                        case (Coordinate2D(x1, y1)) =>
                            //
                            val content: Vector[Int] = Vector.fill(cols)(comp).flatMap(_.toVector)


                            () => new GeneralMessage[Int](content, c.cid)
                        case _ =>
                            throw new Exception(f"Unsupported tbs direction in ${c}")

                    }
                }

            }
        }

        override def tbs_stage(c: Component[Int, Coordinate2D]): () => ComponentMessage = {
            c match {
                case c: ComponentTopo => {
                    c.cid match {
                        case (Coordinate2D(x1, y1)) =>
                            //pre-aggregate
                            val sumValue: Int = comp.sum
                            val content: Vector[Int] = Vector.fill(cols)(sumValue)
                            () => new GeneralMessage[Int](content, c.cid)
                        case _ =>
                            throw new Exception(f"Unsupported tbs direction in ${c}")

                    }
                }

            }
        }


        override def tbr(msg: ComponentMessage): Unit = {
            msg match {
                case x: GeneralMessage[Int] => {
                    for (i<-0 to x.content.size-1){
                        comp(i%cols)  =comp(i%cols) + x.content.asInstanceOf[Vector[Int]](i)
                    }

                }
                case _ => throw new Exception("Unsupported messages!")
            }
        }



    }

    class CompStepFunction(cid: Coordinate2D) extends ComponentTopo(cid) {

    }

    class CompAgent(val comp: ComponentTopo) extends Actor {
        var msgGenerator: Map[Long, () => ComponentMessage] = Map[Long, ()=> ComponentMessage]()
        
        override def run(): Int = {
            receivedMessages.foreach(i => {
                comp.tbr(i.asInstanceOf[ComponentMessage])
            })
            receivedMessages.clear()

            connectedAgentIds.foreach(i => {
                sendMessage(i, msgGenerator(i)())
//                println("ID:"+i+";Message Size"+msgGenerator(i)().asInstanceOf[GeneralMessage[Int]].content.size)
            })


            1
        }
    }

    f"Create a number of tile agents for game of life example" should "run" in {
        val totalComp: Int = 2
        

        val colsPerComp: Int = 1000

        val comps = Range(0, totalComp).map(i => {
            val x = new ComponentTopo(Coordinate2D(i, colsPerComp))
            x.fill()
            x
        })
        
        val agents: IndexedSeq[CompAgent] = comps.map(t => {
            new CompAgent(t)
        })

        if (totalComp > 1) {
            Range(0, totalComp).foreach(j => {
                val i = j.toLong
                agents(j).id = i
                j match {
                    case 0 => agents(j).connectedAgentIds = Vector(i+1)
                    case x if x ==totalComp-1 => agents(j).connectedAgentIds = Vector(i-1)
                    case _ => agents(j).connectedAgentIds = Vector(i-1, i+1)
                }
            })
        }

        agents.foreach(a => {
            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.comp.tbs(comps(i.toInt)))).toMap
        })
//        //pre-compute version
//        agents.foreach(a => {
//            a.msgGenerator = a.connectedAgentIds.map(i => (i, a.comp.tbs_stage(comps(i.toInt)))).toMap
//        })
        val snapshot1 = API.Simulate(agents, 5)
    }
}