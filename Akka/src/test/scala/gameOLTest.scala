package simulation.akka
package test

import org.scalatest.FlatSpec
import simulation.akka.API.Simulate

class gameOLTest extends FlatSpec {
  var role: String = "Standalone"
  var port: Int = 25251

  val width: Int = System.getProperty("width", "500").toInt
  val height: Int = System.getProperty("height", "50").toInt
  val col: Int = System.getProperty("col", "2").toInt
  val row: Int = System.getProperty("row", "2").toInt
  val totalTurns: Int = System.getProperty("totalTurns", "200").toInt
  val mode: Int = System.getProperty("mode", "6").toInt

  println(s"Width: $width")
  println(s"Height: $height")
  println(s"Column: $col")
  println(s"Row: $row")
  println(s"Total turns: $totalTurns")
  println(s"Mode: $mode")

  mode match {
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
////      Simulate.log = null
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
    case 5 => {
      // asyncCall
      val agents = generated.example.gameOfLifeRPC.InitData(width, height)
//      API.OptimizationConfig.mergedWorker()
      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    }
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
////      val agents = generated.example.gameOfLifeTile2DArray.InitData(width, height)
//      val agents = generated.example.gameOfLifeTile2DArray.InitData(width, height)
////      API.OptimizationConfig.mergedWorker()
////      Simulate.log = null
//      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
//    }
//    case 10 => {
//      //tile layout 2DA OR
//      val agents = generated.example.gameOfLifeTile2DArrayOR.InitData(width, height)
//      //      API.OptimizationConfig.mergedWorker()
//      //      Simulate.log = null
//      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
//    }
    case 11 => {
      //tile layout 2DA OR
      val agents = generated.example.gameOfLifeMultiTile.InitData(width, height, col, row)
      //      API.OptimizationConfig.mergedWorker()
      //      Simulate.log = null
      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    }
    case 12 => {
      //tile layout 2DA OR
      val agents = generated.example.gameOfLifeMultiTilePreA.InitData(width, height, col, row)
      //      API.OptimizationConfig.mergedWorker()
      //      Simulate.log = null
      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    }
    case 13 => {
      //tile layout 2DA OR
      val agents = generated.example.gameOfLifeMultiTileObject.InitData(width, height, col, row)
      //      API.OptimizationConfig.mergedWorker()
      //      Simulate.log = null
      val snapshot1 = API.Simulate(agents, totalTurns, role, port)
    }


  }

}
