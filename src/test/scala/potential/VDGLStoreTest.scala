package potential

import base.{Variable, VariableSet}
import org.scalatest.FunSuite
import potential.valueBased.VDGLStore

class VDGLStoreTest extends FunSuite{
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
   val store = VDGLStore(variableSet1, values1)

   // just print the store
   println(store)
   println(store.map.keys.toList.sorted)
}
