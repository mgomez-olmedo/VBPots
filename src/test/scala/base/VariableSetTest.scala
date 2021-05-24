package base

import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Class for making unit tests on VariableSet class
  */
class VariableSetTest extends FunSuite {

   // define variables to use for the tests
   val variableX12 = new Variable("X1", List("x11", "x12"))
   val variableX23 = new Variable("X2", List("x21", "x22", "x23"))
   val variableX34 = new Variable("X3", List("x31", "x32", "x33", "x34"))
   val variableX35 = new Variable("X3", List("x31", "x32", "x33", "x34", "x35"))
   val variableX42 = new Variable("X4", List("x41", "x42"))
   val variableX44 = new Variable("X4", List("x41", "x42","x43","x44"))
   val variableX52 = new Variable("X5", List("x51", "x52"))
   val variableX53 = new Variable("X5", List("x51", "x52", "x53"))
   val variableX64 = new Variable("X6", List("x61", "x62", "x63", "x64"))

   // Creates a variable set with variable1, variable2, variable3 and variable4
   // VSET1 = X1(2) - X2(3) - X3(4) - X4(2)
   val variableSet1 = new VariableSet(List(variableX12, variableX23, variableX34,
                                           variableX42))

   // Creates a variable set with variable3, variable5 and variable2
   // VSET2 = X3(4) - X5(3) - X2(3)
   val variableSet2 = new VariableSet(List(variableX34, variableX53, variableX23))

   // creates the union of variableSet1 and variableSet2
   // UNIONSET = X1(2) - X2(3) - X3(4) - X4(2) - X5(3)
   val unionSet: VariableSet = variableSet1.union(variableSet2)

   // Creates a variable set with variable21, variable22, variable23, variable24
   // variable25 and variable26
   // VSET3 = X1(2) - X2(3) - X3(5) - X4(4) - X5(2) - X6(4)
   val variableSet3 = new VariableSet(List(variableX12, variableX23, variableX35,
                                 variableX44, variableX52, variableX64))

   // test the correct value for possible values
   test("max number of configurations"){
      // VSET1 = 2*3*4*2 = 48
      assert(variableSet1.possibleValues === 48)
      // VSET2 = 4*3*3 = 36
      assert(variableSet2.possibleValues === 36)
   }

   // test computation of weights
   test("weights computation"){
      val weightsSet1 = Array(24, 8, 2, 1)
      val weightsSet2 = Array(9, 3, 1)
      val weightsSet3 = Array(480, 160, 32, 8, 4, 1)

      weightsSet1 should be (variableSet1.weights)
      weightsSet2 should be (variableSet2.weights)
      weightsSet3 should be (variableSet3.weights)
   }

   // test cardinalities
   test("cardinalities computation"){
      val card1 = variableSet1.cardinals
      val card2 = variableSet2.cardinals
      val card3 = variableSet3.cardinals

      // VSET1 = X1(2) - X2(3) - X3(4) - X4(2)
      card1 should be (Array(2, 3, 4, 2))

      // VSET2 = X3(4) - X5(3) - X2(3)
      card2 should be (Array(4, 3, 3))

      // VSET3 = X1(2) - X2(3) - X3(5) - X4(4) - X5(2) - X6(4)
      card3 should be (Array(2, 3, 5, 4, 2, 4))
   }

   // test for difference operation using different variables
   test("test difference operation") {
      // VSET3 = X1(2) - X2(3) - X3(5) - X4(4) - X5(2) - X6(4)
      // VSET1 = X1(2) - X2(3) - X3(4) - X4(2)
      // DIFF = X3(5) - X4(4) - X5(2) - X6(4)
      val dif = variableSet3.difference(variableSet1)
      val difCard = dif.cardinals

      // check difCard
      difCard should be (Array(5, 4, 2, 4))
   }

   // test union operation
   test("test union operation") {
      // VSET1 = X1(2) - X2(3) - X3(4) - X4(2)
      // VSET2 = X3(4) - X5(3) - X2(3)
      // VUNION = X1(2) - X2(3) - X3(4) -X4(2) - X5(3)
      val unionCard = unionSet.cardinals

      // check cardinalities
      unionCard should be (Array(2, 3, 4, 2, 3))
   }

   // test the computation of object sizes
   test("Computation of object sizes test"){
      val variableSet1Size = variableSet1.getMemorySize
      val variableSet2Size = variableSet2.getMemorySize
      val variableSet3Size = variableSet3.getMemorySize

      assert(variableSet1Size != variableSet2Size)
      assert(variableSet3Size > variableSet1Size)
   }

   test("compute relative weights"){
      val relWeightsVS1 = variableSet1.computeRelativeWeights(unionSet)
      val relWeightsVS2 = variableSet2.computeRelativeWeights(unionSet)
      relWeightsVS1 should be (Array(72, 24, 6, 3))
      relWeightsVS2 should be (Array(6, 1, 24))
   }

   test("generate random variable set"){
      val vars = VariableSet.generateRandomSet(5,
         100, 500)
      assert(true)
   }
}
