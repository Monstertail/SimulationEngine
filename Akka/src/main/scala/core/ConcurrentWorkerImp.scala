package simulation.akka.core
package concurrent

import meta.runtime.{Message, Actor}
import scala.collection.mutable.{Map => MutMap}
import scala.concurrent.duration._

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.collection.JavaConversions._

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import java.util.concurrent.atomic.AtomicInteger

class Worker {
    import WorkerSpec._

    private var local_sims: ConcurrentHashMap[Long, Actor] = new ConcurrentHashMap[Long, Actor]()
    private var workerId: Int = 0
    private var totalAgents: Int = 0
    private var totalWorkers: Int = 0
    private val nameMap: ConcurrentHashMap[Long, Int] = new ConcurrentHashMap[Long, Int]()

    private var local_compute_threads: ConcurrentHashMap[Long, ActorRef[LocalAgentSpec.AgentEvent]] = new ConcurrentHashMap[Long, ActorRef[LocalAgentSpec.AgentEvent]]()
    private val peer_workers: ConcurrentHashMap[Int, ActorRef[ReceiveMessages]] = new ConcurrentHashMap[Int, ActorRef[ReceiveMessages]]() 
    private var message_map: Map[Int, Map[Long, List[Message]]] = Map[Int, Map[Long, List[Message]]]()
    // private val receivedMessages: ConcurrentHashMap[Long, ConcurrentLinkedQueue[Message]] = new ConcurrentHashMap[Long, ConcurrentLinkedQueue[Message]]()
    private val receivedMessages = new ConcurrentHashMap[Long, List[Message]]()
    // private val receivedWorkers: ConcurrentLinkedQueue[Int] = new ConcurrentLinkedQueue[Int]()
    private val receivedWorkers = new ConcurrentHashMap[Int, Int]()
    private var expectedWorkerSet: Set[Int] = Set[Int]()
    private var sendToRef: ActorRef[SendTo] = null

    private var acceptedInterval: Int = 0
    private var availability: Int = 1
    private var logicalClock: Int = 0
    private var proposeInterval: Int = Int.MaxValue

    // private var tscontroller: ActorRef[TimeseriesController.LogWorker] = null
    var start: Long = 0
    var end: Long = 0

    private var completedAgents: Int = 0
    private var registeredWorkers: AtomicInteger = new AtomicInteger(0)

    def apply(id: Int, sims: Seq[Actor], totalWorkers: Int): Behavior[WorkerEvent] = Behaviors.setup { ctx =>
        // ctx.log.debug("Register agent with receptionist")
        // val containers: ListBuffer[Actor] = ListBuffer[Actor]()
        local_sims = new ConcurrentHashMap(mapAsJavaMap(sims.map(x => (x.id, x)).toMap))
        totalAgents = sims.size
        this.totalWorkers = totalWorkers
        workerId = id

        // if (ConfigFactory.load("driver-worker").hasPath("driver-containers-per-worker")){
        //     val totalSims = sims.size
        //     val totalGroups: Int = ConfigFactory.load("driver-worker").getValue("driver-containers-per-worker").render().toInt
        //     val simsPerGroup = totalSims / totalGroups
        //     for (i <- Range(0, totalGroups-1)) {
        //         containers.append(newContainer(sims.toList.slice(i*simsPerGroup, (i+1)*simsPerGroup))(true, BoundedLatency))
        //     }
        //     containers.append(newContainer(sims.slice((totalGroups-2), totalSims).toList)(true, BoundedLatency))
        //     local_compute_threads = new ConcurrentHashMap(mapAsJavaMap(containers.map(a => (a.id, ctx.spawn((new LocalAgent).apply(a), f"simAgent${a.id}"))).toMap))
        // } else {
        local_compute_threads = new ConcurrentHashMap(mapAsJavaMap(sims.map(a => (a.id, ctx.spawn((new LocalAgent).apply(a), f"simAgent${a.id}"))).toMap))
        // }

        if (totalWorkers > 1){
            ctx.system.receptionist ! Receptionist.Register(WorkerUpdateAgentMapServiceKey, ctx.self)

            val workerSub = ctx.messageAdapter[Receptionist.Listing] {
                case WorkerUpdateAgentMapServiceKey.Listing(workers) =>
                    if (workers.size == totalWorkers){
                        workers.filter(i => i!= ctx.self).foreach(w => {
                            w ! ReceiveAgentMap(workerId, local_sims.keys().toSeq.asInstanceOf[Iterable[java.lang.Long]], ctx.self)
                        })
                    }
                    Prepare()
            }       
            
            ctx.system.receptionist ! Receptionist.Subscribe(WorkerUpdateAgentMapServiceKey, workerSub)
        } else {
            ctx.system.receptionist ! Receptionist.Register(WorkerStartServiceKey, ctx.self)
            ctx.system.receptionist ! Receptionist.Register(WorkerStopServiceKey, ctx.self)
        }  

        // ctx.system.receptionist ! Receptionist.Subscribe(TimeseriesController.tsControllerServiceKey, workerSub)
        worker()
    }

    // Consider replacing receivedWorkers with a total workers
    private def worker(): Behavior[WorkerEvent] =
        Behaviors.receive[WorkerEvent] { (ctx, message) =>
            message match {
                case Prepare() => 
                    worker()

                case ReceiveAgentMap(wid, nameIds, reply) => 
                    if (!peer_workers.containsKey(wid)){
                        val total = registeredWorkers.addAndGet(1)
                        nameIds.foreach(n => {
                            nameMap.putIfAbsent(n, wid)
                        })
                        peer_workers.putIfAbsent(wid, reply)
                        if (total == totalWorkers - 1){
                            // println(workerId +  f" peer workers ${peer_workers.keys.map(i => i.toString).toList}")
                            ctx.system.receptionist ! Receptionist.Register(WorkerStartServiceKey, ctx.self)
                            ctx.system.receptionist ! Receptionist.Register(WorkerStopServiceKey, ctx.self)
                        }
                    }
                    Behaviors.same

                case ReceiveMessages(wid, messages) =>
                    val start = System.currentTimeMillis()
                    ctx.log.debug(f"Worker ${workerId} receives ${messages.size} messages from worker ${wid}")
                    
                    receivedWorkers.computeIfAbsent(wid, x => {
                        for (m <- messages) {
                            receivedMessages.update(m._1, receivedMessages.getOrElse(m._1, List()) ::: m._2)
                        }
                        0
                    })
                    receivedWorkers.putIfAbsent(wid, 0)
                    if (receivedWorkers.keys().toSet == expectedWorkerSet){
                        ctx.self ! Start()
                    }
                    val end = System.currentTimeMillis()
                    // ctx.log.info(f"Worker ${workerId} processes message takes ${end-start} ms")
                    worker()
                    
                case Start() =>
                    ctx.log.debug(f"Worker ${workerId} starts! Received from ${receivedWorkers.keys().toSet}")
                    start = System.currentTimeMillis()
                    receivedWorkers.clear()
                    expectedWorkerSet = Set[Int]()

                    if (totalAgents > 0){
                        local_sims.foreach(a => {
                            a._2.time += acceptedInterval
                            val x = receivedMessages.remove(a._1)
                            if (x != null) {
                                a._2.receivedMessages.addAll(x)
                            }
                        })

                        proposeInterval = Int.MaxValue
                        ctx.spawnAnonymous(
                            new Aggregator[LocalAgentSpec.MessagesAdded, AgentsCompleted](
                            sendRequests = { replyTo =>
                                local_compute_threads.forEach((k, v) => {
                                    v ! LocalAgentSpec.AddMessages(replyTo)                                   
                                })
                            },
                            expectedReplies = local_compute_threads.size,
                            ctx.self,
                            aggregateReplies = replies => {
                                var collectedMessages: MutMap[Long, List[Message]] = MutMap[Long, List[Message]]()
                                for (r <- replies) {
                                    r.indexedSentMessages.foreach(i => {
                                        collectedMessages.update(i._1, collectedMessages.getOrElse(i._1, List[Message]()) ::: i._2) 
                                    })
                                    if (r.proposeInterval < proposeInterval){
                                        proposeInterval = r.proposeInterval
                                    }
                                }
                                // Deliver local messages to agents' mailboxes
                                collectedMessages.filterKeys(x => local_sims.get(x)!=null).foreach(i => {
                                    local_sims.get(i._1).receivedMessages.addAll(collectedMessages.remove(i._1).get)
                                })
                                message_map = collectedMessages.toMap.groupBy(i => nameMap.getOrElse(i._1, workerId))
                                AgentsCompleted()
                            },
                            timeout=1000.seconds).apply())
                    } else {
                        ctx.self ! AgentsCompleted()
                    }
                    worker()

                case ExpectedReceives(receive_map, replyTo, acceptedInterval, availability) => 
                    ctx.log.debug(f"Worker ${workerId} receives expected-from list")
                    val remoteWorkers = message_map.keys.filter(i => i!=workerId).toSet
                    remoteWorkers.foreach(i => {
                        peer_workers.get(i) ! ReceiveMessages(workerId, message_map(i).asInstanceOf[Map[java.lang.Long, List[Message]]])
                    })         
                    sendToRef = replyTo       
                    this.acceptedInterval = acceptedInterval
                    this.availability = availability
                    logicalClock += acceptedInterval
                    expectedWorkerSet = receive_map.getOrElse(workerId, Set[Int]())
                    if (receivedWorkers.keys().toSet == expectedWorkerSet){
                        ctx.self ! Start()
                    } 
                    worker()
                    
                case AgentsCompleted() =>
                    end = System.currentTimeMillis()
                    ctx.log.debug(f"Worker ${workerId} runs for ${end-start} ms")
                    val remoteWorkers = message_map.keys.filter(i => i!=workerId).toSet
                    sendToRef ! SendTo(workerId, remoteWorkers, proposeInterval)
                    // todo: replace later with materialization strategy, rather than checking for null
                    // if (simulation.akka.API.Simulate.log != null){
                    //     simulation.akka.API.Simulate.log.add(logicalClock, local_sims.map(_._2.SimClone()))
                    // }
                    // Cannot send messages to other workers immediately, race condition
                    completedAgents = 0
                    Behaviors.same

                case Stop() =>
                    ctx.log.debug(f"Stop worker ${workerId}")
                    local_compute_threads.forEach((k, v) => ctx.stop(v))
                    Behaviors.stopped
            }
        }
}
