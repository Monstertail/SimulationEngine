package simulation.base
package test

import API._
import org.scalatest.FlatSpec
//import simulation.akka.API.Simulate

class gameOLTest extends FlatSpec {
//  val width = 100
//  val height: Int = 1000
//  val totalTurns: Int = 200
//  val mode: Int = 10
  var role: String = "Standalone"
  var port: Int = 25251

  val width: Int = System.getProperty("width", "1000").toInt
  val height: Int = System.getProperty("height", "100").toInt
  val totalTurns: Int = System.getProperty("totalTurns", "200").toInt
  val mode: Int = System.getProperty("mode", "6").toInt

  "gameOfLife" should "run with given arguments" in {
    // Use the width, height, totalTurns, and mode variables in your test
    //    case 1 => {
    //      // Messaging
    //      val agents = generated.example.gameOfLife.InitData(width, height)
    //      API.OptimizationConfig.mergedWorker()
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }
    //    case 2 => {
    //      // callAndForget
    //      val agents = generated.example.gameOfLifeRPCOneSide.InitData(width, height)
    //      API.OptimizationConfig.mergedWorker()
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }
    //
    //    case 3 => {
    //      // Direct method call, double buffer
    //      val agents = generated.example.gameOfLifeRPCOneSideDoubleBuffer.InitData(width, height)
    //      API.OptimizationConfig.directMethodCall()
    //      Simulate.log = null
    //      val snapshot1 = API.Simulate(agents, 2 * totalTurns, role, port)
    //    }
    //
    //    case 4 => {
    //      // Direct method call, multi-version
    //      val agents = generated.example.gameOfLifeRPCOneSideMultiversion.InitData(width, height)
    //      API.OptimizationConfig.directMethodCall()
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }
    //
    //
    //    case 5 => {
    //      // asyncCall
    //      val agents = generated.example.gameOfLifeRPC.InitData(width, height)
    //      API.OptimizationConfig.mergedWorker()
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }
    //
    //    case 6 => {
    //      // Messaging, concurrent
    //      val agents = generated.example.gameOfLife.InitData(width, height)
    //      API.OptimizationConfig.concurrentWorker()
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }
    //
    //    case 7 => {
    //      //tile layout(default:list)
    //      val agents = generated.example.gameOfLifeTile.InitData(width, height)
    //      API.OptimizationConfig.mergedWorker()
    //      Simulate.log = null
    //      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    //    }

    //    case 8 => {
    //      //tile layout 2D
    //      val agents = generated.example.gameOfLifeTile2DArray.InitData(width, height)
    ////      API.OptimizationConfig.mergedWorker()
    ////      Simulate.log = null
    //      val snapshot1 = API.Simulate(agents, totalTurns)
    //    }
//    case 9 => {
//      //tile layout 1D
//      val agents = generated.example.gameOfLifeTile1DArray.InitData(width, height)
//      //      API.OptimizationConfig.mergedWorker()
//      //      Simulate.log = null
//      val snapshot1 = API.Simulate(agents, totalTurns)
//    }

    case 10 => {
      //tile layout 2DA OR
      val agents = generated.example.gameOfLifeTile2DArrayOR.InitData(width, height)
      //      API.OptimizationConfig.mergedWorker()
      //      Simulate.log = null
      val snapshot1 = API.Simulate(agents, totalTurns)
    }

  }




}


