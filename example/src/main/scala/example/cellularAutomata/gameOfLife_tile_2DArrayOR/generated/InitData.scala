package generated.example.gameOfLifeTile2DArrayOR

object InitData  {
    
        def apply(width: scala.Int, height: scala.Int): scala.`package`.List[example.`package`.Actor] = {
  val totalComp: scala.Int = 1;
  val array2D = new example.gameOfLifeTile2DArrayOR.Array2D(width, height);
  array2D.init();
  val tile = new Tile(width, height, array2D);
  val components = scala.Predef.intWrapper(1).to(totalComp).map[generated.example.gameOfLifeTile2DArrayOR.Tile, scala.collection.immutable.IndexedSeq[generated.example.gameOfLifeTile2DArrayOR.Tile]](((x: Int) => tile))(scala.collection.immutable.IndexedSeq.canBuildFrom[generated.example.gameOfLifeTile2DArrayOR.Tile]).toList;
  components
} 
        
        def wrapper(args: List[Any]): List[example.Actor] = {
          apply(args(0).asInstanceOf[Int],args(1).asInstanceOf[Int])
        }
        
        def writeSchema(pw: java.io.PrintWriter): Unit = {
          pw.write("width,height")
          pw.flush()
        }
        
        
}