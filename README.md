[![Scala CI](https://github.com/ZiluTian/economic_simulations/actions/workflows/scala.yml/badge.svg?branch=latest)](https://github.com/ZiluTian/economic_simulations/actions/workflows/scala.yml)

## Large-scale agent-based simulation 

The goal of this project is to provide a framework suitable for scaled-out agent-based simulation. 

### Overview 
- [Synchronization DSL](#DSL)
- [Sims as Meta-Programs](#Meta-programs)
- [Start Simulation](#Simulation)
- [Folder Structure](#Folder)
- [Issues and Solutions](docs/Issues.md)

### <a name="DSL"></a> Synchronization DSL

We define an embedded DSL to address the synchronization issue among Sims. 

The syntax for a blocking message is the same as a regular function. When a Sim calls the public method of another, it sends a blocking message to the receiver, waiting for its response. You can define in Sim1 a call to Sim2:  

```val secret: Int = Sim2.tellMeThis()``` or ```val workDone: Boolean = Sim2.doThat()``` 

given that `tellMeThis()` and `doThat()` are public methods in Sim2's class. The transpiler converts it to delivering a message to Sim2 and waiting for a reply. When the reply arrives, the function returns and Sim1's variables ```secret``` and ```workDone```  get their values. 

Besides blocking calls, Sim can also send asynchronous messages, with a different syntax```asyncMessage(() => msg)```. For asynchronous messaging, the Sim places the message in its mailbox and continues. Messages are not delivered immediately. 

Function ```waitLabel``` signals that messages in the Sim's mailbox are ready to be delivered. 

Apart from communicating with others, Sim decides when it wants to check its mailbox by calling another DSL function, ```handleMessages```. We also have another special instruction, `interrupt`.  You can find the signature of these methods here ```/src/main/scala/meta/classLifting/SpecialInstructions.scala```

### <a name="Meta-Programs"></a> Sims as Meta-Programs
The embedded DSL is in a staged meta-programming environment. Staging is the operation that generates **object programs** from **meta-programs**. In our framework, users define the behaviour of each agent in **meta-programs** written in a subset of Scala enriched with DSL, which are then translated by our transpiler to **object programs** (valid Scala source programs).
 
We use Squid meta-programming framework. As of now, you need to build Squid locally (branch: class-lifting). 

```
- clone the class-lifting branch of Squid repo: 
  https://github.com/epfldata/squid.git
- create a local snapshot by running publishLocal: 
  bash bin/publishLocal.sh
- Expected output: 
  you should see "build 0.4.1-SNAPSHOT"
```
   
   Here are some tips for writing meta-programs in this framework: 
* The optimzations created work for specific use-cases:
  * ActorMerge takes a pair of ActorType Names to specify which one to merge.  
  Take care, that the class variables are named differently in the two Sims
  * Stateless Server Optimization has following rules:
    * A stateless server here means a class whose methods don't change any of its attributes, because then those methods can be copied to other Sims.
    * A stateless server class should not have a wait in a non-blocking method, otherwise the program will not behave as the original
    * A stateless server cannot call a method from a non-stateless Server
    * An object can have only one reference to a (unique) specific stateless server class.
    It can have different references to different stateless servers(if those servers have unique attribute names among themselves).
    * The optimization has to be applied before using the EdgeMerge Optimization, since it requires the original graph
* Following structures are lifted by default: 
  * while(cond) {code}
  * val x = code
  * list.foreach(code)
  * list.map(code)
  * list.flatMap(code)
  * (x <: Actor).\[methodCall\](...)
  * if(cond) code else code2
* You can use any other code, which does not contain a nested supported lifting structure.   
* You can extend the lifting by:
  * creating a subclass of algo
  * add in this subclass the nodes to the graph
  * Extend the lifter and override the method liftCodeOther  
  and handle there your created algos.
* To lift a class, annotate it with @lift and extend from runtime.Actor


### <a name="Simulation"></a> Start Simulation 
 Once you have written your meta-programs and translated them to object programs, you can start simulation. The object programs are in folder `/generated/src/main/scala/$packageName$`. To start a simulation, first define you simulation configuration: the total turns and time you want to run the simulation for. Next, decide whether you want to use Spark to parallelize your agents. When running locally, Spark consumes more memory and often results in slower simulation. Afterwards, simply pass the configuration to the simulation driver and invoke `run()`. 
```
// Define the configuration for your simulation
val c: SimulationConfig = SimulationConfig(totalTurn=3)
// Run the initialization method 
generated.*yourExample*.initActors()
// Add the configuration to the simulation 
val result: SimulationSnapshot = new Default(c).run()
// Pass the configuration to Spark simulation 
val result2: SimulationSnapshot = new SimulationSpark(c).run()
```
 
 You can also reference the test scripts in `/src/test/scala`
 
### <a name="Folder"></a> Folder Structure 
- `ecosim/` contains the legacy implementation of the simulation framework without using message passing 
- `example/` contains the examples using class-lifting and message-passing 
- `generated/` contains the object programs. The simulation drivers take object programs and run 
- `lib/` contains the library for writing the meta-programs of a simulation 
    - `Bot/` are the agents which you can instantiate directly in your example. Please refer to the rumor example which uses the LoggerBot to see how to use it. Please make sure to include the Bot you used in your main class when compiling your example. 
    - Other folders contain helper classes which are non-agent that you can use directly in your example. 
- `src/` contains the transpiler source code and supporting runtime objects. 

 
### Code Format
We use Scalafmt for formatting the code.
For installing Scalafmt with your favorite IDE see https://scalameta.org/scalafmt/
 
Please open an issue or make a pull request if you encounter other problems or have suggestions. Thank you.  
