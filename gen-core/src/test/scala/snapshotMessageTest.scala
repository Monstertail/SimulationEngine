package generated.core

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import meta.deep.IR.TopLevel.ClassWithObject
import meta.deep.IR
import meta.runtime.{Actor}
import meta.API._
import org.scalatest.FlatSpec
import scala.util.Random

class SnapshotMessageTest extends FlatSpec {
    import meta.deep.IR.Predef._

    "The snapshot" should "capture messages not processed in base messaging layer" in {
        val agents = generated.core.test.snapshot.InitData()
        val c = new SimulationConfig(agents, 1)
        val snapshot1 = StartSimulation[BaseMessagingLayer.type](c)
        // After 1 epoch, we have 7 messages, sent by sender
        assert(snapshot1.messages.length == 7)
        // After 2 epochs, we have 14 messages. 7 replies from receiver and 7 from the sender. 
        val snapshot2 = StartSimulation[BaseMessagingLayer.type](new SimulationConfig(snapshot1.sims, 1, messages = snapshot1.messages))
        assert(snapshot2.messages.length == 14)
        // In the later epochs, the message load of the system remains at 14 messages, as previously
        val snapshot3 = StartSimulation[BaseMessagingLayer.type](new SimulationConfig(snapshot2.sims, 10, messages = snapshot2.messages))
        assert(snapshot3.messages.length == 14)
    }

    "The snapshot" should "capture messages not processed in Akka messaging layer" in {
        val agents = generated.core.test.snapshot.InitData()
        val c = new SimulationConfig(agents, 1)
        val snapshot1 = StartSimulation[AkkaMessagingLayer.type](c)
        // After 1 epoch, we have 7 messages, sent by sender
        assert(snapshot1.messages.length == 7)
        assert(snapshot1.sims.length == 8)

        // After 2 epochs, we have 14 messages. 7 replies from receiver and 7 from the sender.         
        val snapshot2 = StartSimulation[AkkaMessagingLayer.type](new SimulationConfig(snapshot1.sims, 1, messages = snapshot1.messages))
        assert(snapshot2.messages.length == 14)
        assert(snapshot2.sims.length == 8)

        // In the later epochs, the message load of the system remains at 14 messages, as previously
        val snapshot3 = StartSimulation[AkkaMessagingLayer.type](new SimulationConfig(snapshot2.sims, 10, messages = snapshot2.messages))
        assert(snapshot3.messages.length == 14)
        assert(snapshot3.sims.length == 8)
    }

    "The snapshot" should "capture messages not processed in Spark messaging layer" in {
        val agents = generated.core.test.snapshot.InitData()
        val c = new SimulationConfig(agents, 1)
        val snapshot1 = StartSimulation[SparkMessagingLayer.type](c)
        // After 1 epoch, we have 7 messages, sent by sender
        assert(snapshot1.messages.length == 7)
        assert(snapshot1.sims.length == 8)

        // // After 2 epochs, we have 14 messages. 7 replies from receiver and 7 from the sender.         
        val snapshot2 = StartSimulation[SparkMessagingLayer.type](new SimulationConfig(snapshot1.sims, 1, messages = snapshot1.messages))
        assert(snapshot2.messages.length == 14)
        assert(snapshot2.sims.length == 8)

        // In the later epochs, the message load of the system remains at 14 messages, as previously
        val snapshot3 = StartSimulation[SparkMessagingLayer.type](new SimulationConfig(snapshot2.sims, 10, messages = snapshot2.messages))
        assert(snapshot3.messages.length == 14)
        assert(snapshot3.sims.length == 8)
    }
}