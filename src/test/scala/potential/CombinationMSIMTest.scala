package potential

import base.{Variable, VariableSet}
import org.scalatest.FunSuite

class CombinationMSIMTest extends FunSuite {
   val cores: Int = Runtime.getRuntime.availableProcessors
   println("cores: " + cores)

   // creates variables for domains
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32", "x33", "x34", "x35"))
   val variable4 = new Variable("X4", List("x41", "x42", "x43", "x44"))

   // Creates a variable set with variable1, variable2, variable3 and variable4
   val variableSet1 = new VariableSet(List(variable1, variable2, variable3))

   // Creates a variable set with variable3, variable5 and variable2
   val variableSet2 = new VariableSet(List(variable2, variable4, variable1))

   // define the values to use in a potential of X1(2) and X2(3), X3(5)
   val valuesPot1 = Array(0, 0, 0.2, 0.3, 0.5,
      1, 0, 0, 0, 0,
      0.7, 0.1, 0.1, 0.1, 0,
      0.5, 0.3, 0.2, 0, 0,
      0.2, 0.2, 0.1, 0.4, 0.1,
      0, 0.7, 0.3, 0, 0)
   // define the values to use in a potential of X2(3), X4(4) and X1(2)
   val valuesPot2 = Array(0.3, 0.7,
      0.1, 0.9,
      0, 1,
      1, 0,
      0.5, 0.5,
      0.3, 0.7,
      0.2, 0.8,
      0.4, 0.6,
      0, 0,
      0, 0,
      0, 0,
      0, 0)

   // creates representation as tables
   val storeTable1 = TableStore(variableSet1, valuesPot1)
   val storeTable2 = TableStore(variableSet2, valuesPot2)
   val potential1Table = Potential(storeTable1)
   val potential2Table = Potential(storeTable2)

   // makes combination as tables
   potential1Table.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultTable = potential1Table.combine(potential2Table)
   println("result as table: ")
   println(resultTable)

   // convert potential to ArrayIMStore, ArraySIMStore, MapLIMStore
   val store1: Potential = potential1Table.convert(ValueStoreTypes.IDPMUT)
   val store2 = potential2Table.convert(ValueStoreTypes.IDPMUT)

   // sets the list of combiners and marginalizers to use
   // for ArraIMStore
   var combiners = List(OperatorType.DEFAULT, OperatorType.ALT1,
                        OperatorType.ALT2, OperatorType.ALT3,
                        OperatorType.ALT4, OperatorType.ALT5)
   var marginalizers = List(OperatorType.DEFAULT, OperatorType.ALT1,
                        OperatorType.ALT2)

   val store7 = potential1Table.convert(ValueStoreTypes.VDISETMUT)
   val store8 = potential2Table.convert(ValueStoreTypes.VDISETMUT)

   // test all the combinations of operators on ArraSIMStore
   for(comb <- combiners; marg <- marginalizers){
      test(s"combination MSIM (comb $comb - marg $marg)"){
         println(s"combination test MSIM: (comb $comb - marg $marg)")
         // combine as array of indexes
         store7.setFunctions(comb, marg)

         val storeResult = store7.combine(store8)
         println("result as map of set of indexes: ")
         println(storeResult)

         // compare both potentials
         val result = (resultTable == storeResult)

         // check the result of the comparison
         assert(result == true)
      }
   }
}
