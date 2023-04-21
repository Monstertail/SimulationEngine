package generated.example.gameOfLifeTile2DArrayOR

class Tile(val width : Int, val height : Int, val array2D : example.gameOfLifeTile2DArrayOR.Array2D) extends meta.runtime.Actor {


  private var  reflectionIR_18: Int = -1
private var resetData_0: scala.Any = null
private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]()
private var resetData_2: meta.runtime.ResponseMessage = null
private var bindingMut_3: scala.Int = 0
private var bindingMut_4: example.gameOfLifeTile2DArrayOR.Array2D = null
private var bindingMut_5: example.gameOfLifeTile2DArrayOR.Array2D = null
private var unblockFlag_6: scala.Boolean = true
private var positionVar_7: scala.Int = 0
private 
  val commands_31 = (() => {
  val data_8 = new scala.Array[scala.Function0[scala.Unit]](13);
  data_8.update(0, (() => positionVar_7 = 1));
  data_8.update(1, (() => {
    this.handleRPC();
    resetData_0 = ();
    val x_9 = this.array2D;
    resetData_0 = x_9;
    val x_10 = resetData_0;
    val x_11 = x_10.asInstanceOf[example.gameOfLifeTile2DArrayOR.Array2D];
    bindingMut_5 = x_11;
    val x_12 = bindingMut_5;
    x_12.update();
    resetData_0 = ();
    val x_13 = this.array2D;
    resetData_0 = x_13;
    val x_14 = resetData_0;
    val x_15 = x_14.asInstanceOf[example.gameOfLifeTile2DArrayOR.Array2D];
    bindingMut_4 = x_15;
    val x_16 = bindingMut_4;
    x_16.swap_ref();
    resetData_0 = ();
    val x_17 = this.time;
    val x_18 = x_17.+(1);
    resetData_0 = x_18;
    val x_19 = resetData_0;
    val x_20 = x_19.asInstanceOf[scala.Int];
    bindingMut_3 = x_20;
    positionVar_7 = 2
  }));
  data_8.update(2, (() => {
    val x_21 = this.time;
    val x_22 = bindingMut_3;
    val x_23 = x_22.-(x_21);
    this.`proposeInterval_=`(x_23);
    resetData_0 = ();
    positionVar_7 = 3;
    unblockFlag_6 = false
  }));
  data_8.update(3, (() => {
    val x_24 = this.time;
    val x_25 = bindingMut_3;
    val x_26 = x_24.<(x_25);
    if (x_26)
      positionVar_7 = 2
    else
      positionVar_7 = 4
  }));
  data_8.update(4, (() => {
    val x_27 = this.time;
    val x_28 = bindingMut_3;
    val x_29 = x_27.<(x_28);
    val x_30 = x_29.`unary_!`;
    if (x_30)
      positionVar_7 = 5
    else
      ()
  }));
  data_8.update(5, (() => positionVar_7 = 1));
  data_8.update(6, (() => positionVar_7 = 7));
  data_8.update(7, (() => {
    positionVar_7 = 8;
    unblockFlag_6 = false
  }));
  data_8.update(8, (() => positionVar_7 = 7));
  data_8.update(9, (() => positionVar_7 = 7));
  data_8.update(10, (() => {
    this.handleRPC();
    resetData_0 = ();
    positionVar_7 = 11
  }));
  data_8.update(11, (() => {
    positionVar_7 = 12;
    unblockFlag_6 = false
  }));
  data_8.update(12, (() => positionVar_7 = 11));
  data_8
}).apply();
  

  override def run(): Int = {
    
    sendMessages.clear()
    unblockFlag_6 = true
    while (unblockFlag_6 && (positionVar_7 < 13)) {
      commands_31(positionVar_7)()
    }
    proposeInterval
  }

override def SimClone(except_variables: Set[String]): Tile = {
  val newAgent = new Tile(width, height, array2D)

  newAgent
}

override def SimReset(preserved_names: Set[String]): Unit = {
  positionVar_7 = 0
  time = 0
  
}

}
