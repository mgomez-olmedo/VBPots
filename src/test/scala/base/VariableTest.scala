package base

import org.scalatest.{BeforeAndAfter, FunSuite}
import utils.DataSizes

/**
  * Simple test for Variable class
  */
class VariableTest extends FunSuite {

   // Creates a new Variable
   val variable = new Variable("X2", List("x21", "x22", "x23"))

   // just checks if the number of states is properly stored
   test("Storage of number of states") {
      assert(variable.getNumberStates === 3)
   }

   test("Test object size") {
      val objectSize = variable.getMemorySize
      val directComputation = DataSizes.VARIABLE
      assert(objectSize == directComputation)
   }
}
