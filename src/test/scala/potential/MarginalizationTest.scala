package potential

import base.{Variable, VariableSet}
import org.scalatest.FunSuite

class MarginalizationTest extends FunSuite {
   val cores: Int = Runtime.getRuntime.availableProcessors
   println("cores: " + cores)

   // creates variables for domains
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32"))
   val variable4 = new Variable("X4", List("x41", "x42", "x43"))

   // Creates a variable set with variable1, variable2, variable3
   // and variable4
   val variableSet1 = new VariableSet(List(variable1, variable2, variable3,
                                       variable4))

   // define the values to use in a potential of X1(2) and X2(3), X3(2)
   // and X4(3)
   val valuesPot1 = Array(
      0.1, 0.9, 0,
      0.2, 0.3, 0.5,
      0.1, 0.1, 0.8,
      1, 0, 0,
      0.5, 0.3, 0.2,
      0.6, 0.3, 0.1,
      1, 0, 0,
      0, 0, 1,
      0.2, 0.1, 0.7,
      0.3, 0.6, 0.1,
      0.2, 0.2, 0.6,
      0.4, 0.6, 0
   )

   // creates representation as tables
   val storeTable1 = TableStore(variableSet1, valuesPot1)
   val potential1Table = Potential(storeTable1)

   // makes marginalization as tables
   potential1Table.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultTable = potential1Table.marginalize(variable3)
   println("result as table: ")
   println(resultTable)

   // convert potential to ArrayIMStore
   val store1: Potential = potential1Table.convert(ValueStoreTypes.IDPMUT)

   // sets the list of combiners and marginalizers to use
   var marginalizers = List(OperatorType.DEFAULT,
                           OperatorType.ALT1,
                           OperatorType.ALT2)

   // test all the marginalization methods
   for(marg <- marginalizers){
      test(s"marginalization AIM (marg $marg)"){
         println(s"marginalization test AIM: (marg $marg)")
         // combine as array of indexes
         store1.setFunctions(OperatorType.DEFAULT, marg)

         val storeResult = store1.marginalize(variable3)
         println("result as array of indexes " + marg + " : ")
         println(storeResult)

         // compare both potentials
         val result = (resultTable == storeResult)

         // check the result of the comparison
         assert(result == true)
      }
   }

   // set marginalizers to check for ASIM store
   marginalizers = List(OperatorType.DEFAULT,
      OperatorType.ALT1,
      OperatorType.ALT2)

   // convert potential to ArraySIMStore
   val store2: Potential = potential1Table.convert(ValueStoreTypes.IDSETMUT)

   // test all the marginalization methods
   for(marg <- marginalizers){
      test(s"marginalization ASIM (marg $marg)"){
         println(s"marginalization test ASIM: (marg $marg)")
         // combine as array of indexes
         store2.setFunctions(OperatorType.DEFAULT, marg)

         val storeResult = store2.marginalize(variable3)
         println("result as array of indexes " + marg + " : ")
         println(storeResult)

         // compare both potentials
         val result = (resultTable == storeResult)

         // check the result of the comparison
         assert(result == true)
      }
   }

   // convert potential to ArraySIMStore
   val store3: Potential = potential1Table.convert(ValueStoreTypes.VDILISTMUT)

   // test all the marginalization methods
   for(marg <- marginalizers){
      test(s"marginalization MLIM (marg $marg)"){
         println(s"marginalization test MLIM: (marg $marg)")
         // combine as array of indexes
         store3.setFunctions(OperatorType.DEFAULT, marg)

         val storeResult = store3.marginalize(variable3)
         println("result as array of indexes " + marg + " : ")
         println(storeResult)

         // compare both potentials
         val result = (resultTable == storeResult)

         // check the result of the comparison
         assert(result == true)
      }
   }
}
