package potential

import base.{Variable, VariableSet}
import bnet.Bnet
import org.scalatest.FunSuite
import potential.indexBased.IDPMStore

class IDPMStoreTest extends FunSuite {
   // creates variables for domains
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32"))

   // Creates a variable set with variable1, variable2, variable3 and variable4
   val variableSet1 = new VariableSet(List(variable1, variable2, variable3))

   // define the values to use in a potential of X1(2) and X2(3), X3(5)
   val values1 = Array(0.1, 0.9, 0.5, 0.5, 0, 1,
      0.8, 0.2, 0.2, 0.8, 0.9, 0.1)

   // creates the store
   val store = IDPMStore(variableSet1, values1)

   // just print the store
   println(store)
   println("values of the store: ")
   println(store.values.sorted.foreach(value => print(value + " ")))
   println("\n----- end of test setup -----------------")

   /**
    * checks the merge operation reduced the number of
    * stored values
    */
   test("checks merge of two values") {
      // as the mthod will modify store, gets initially the number
      // of values
      val initialNumberValues = store.getDifferentValues.length

      // check the method for merging two values
      val merged = store.merge(0.9, 1)
      println()
      println("Store after merging..............")
      println(merged)
      val different = merged.getDifferentValues.length
      assert(different == initialNumberValues - 1)
   }

   /**
    * test of prune operation on the store created at the
    * beginning
    */
   test("checks prune operation for artificial potential") {
      println()
      println("start of prune check")
      println(store)
      val result = store.prune(0.05)
      println("result of pruning")
      println(result)
   }

   /**
    * test for making a prune operation on a real potential from
    * a bnlearn network
    */
   test("test of prune on potential of hepar2 potential") {
      // creates the net
      val net = Bnet("pathfinder.net")

      // gets the required potential
      val basePotential = net.getPotentialForVariable("F39")

      // convert it to VDGLStore
      val store = basePotential.convert(ValueStoreTypes.IDPMSTORE).store.asInstanceOf[IDPMStore]
      println("-------------------- original store -----------------")
      println(store)
      println(".....................................................")

      // just prune with a low threshold
      val result = store.prune(0.1)
      println("-------------------- pruned store -------------------")
      print(result)
   }
}