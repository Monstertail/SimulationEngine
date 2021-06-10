package example

import custMacros.Sim
import org.coroutines._
import meta.classLifting.SpecialInstructions._
import meta.runtime.Message
import meta.runtime.simulation.{SimulationConfig, InTimeBase}

@Sim
class C() extends Actor {
    def apiMtd1(foo: Int): Int = foo

    def main(): Unit = {
        while (true){
            handleMessages()
            waitLabel(Turn, 1)
        }
    }
}

@Sim
class B(val a: Int) extends Actor {

    val bar: Int = 100
    
    var foo: String = "Hello world!"

    var neighbor: C = null

    private def apiMtd2(b: Int): Unit = {
        println("Hello world!")
    }

    def apiMtd1(aa: Int, f: String): Unit = {
        println("Secret is " + f)
        apiMtd2(aa)
    }

    def helper: Unit = { println("This is a helper method!") }

    def helper2(): Unit = { println("This is a helper method!") }

    def main(): Unit = {
        while (true) {

            var f = asyncMessage[Int](() => neighbor.apiMtd1(100))

            while (!f.isCompleted){
                waitLabel(Turn, 1)
                handleMessages()
            }

            val reply = f.value.get
            println("Receive reply! " + reply)

            waitLabel(Turn, 1)
            handleMessages()
        }
    }
}

object Test extends App {
    val b = new B(4)
    val c = new C()

    b.neighbor = c

    val config = new SimulationConfig(List(b, c), totalTurn=10)

    val base = new InTimeBase(config)
    base.run()
}