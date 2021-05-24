package mapper

import base.{Configuration, Variable, VariableSet}
import org.scalatest.FunSuite

/**
 * Class for testing the methods of Configuration class. The
 * test is based on defining a domain with variables X1 (2
 * states), X2 (3 states), X3 (4 states), X4 (2 states) and
 * X5 (3 states). This domain produces the following vector
 * of weights: 72, 24, 6, 3, 1
 * The last variable is the one with lower weight
 */
class CombineMapperTest extends FunSuite {
   // Create variables
   /**
    * Set of variables defining the configuration:
    * X1, X2, X3, X4, X5
    */
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32", "x33", "x34"))
   val variable4 = new Variable("X4", List("x41", "x42"))
   val variable5 = new Variable("X5", List("x51", "x52", "x53"))

   // Weights vector
   // 72 24 6 3 1

   val partial1 = new VariableSet(List(variable1, variable3,
               variable4, variable5))

   val partial2 =   new VariableSet(List(variable1, variable2,
               variable4))

   // creates a mapper
   val mapper = CombineMapper(partial1, partial2)

   // gets tge global domain
   val globalSet = mapper.resultDomain
   println("global domain: " + globalSet)

   /**
    * test compatibility function with positive result
    */
   test("test for compatibility (positive result") {
      val conf1 = Configuration(partial1)
      val conf2 = Configuration(partial2)
      val newConf1 = conf1.setValues(Array(1, 2, 1, 1))
      val index1 = newConf1.computeIndex
      val newConf2 = conf2.setValues(Array(1, 1, 1))
      val index2 = newConf2.computeIndex
      println("compatible positive: " + newConf1 + " index: " + index1)
      println("compatible positive: " + newConf2 + " index: " + index2)

      // check compatibility
      assert(mapper.compatible(index1, index2) == true)
   }

   /**
    * test compatibility function with negative result
    */
   test("test for compatibility (negative result") {
      val conf1 = Configuration(partial1)
      val conf2 = Configuration(partial2)
      val newConf1 = conf1.setValues(Array(1, 3, 0, 2))
      val index1 = newConf1.computeIndex
      val newConf2 = conf2.setValues(Array(1, 2, 1))
      val index2 = newConf2.computeIndex
      println("compatible negative: " + newConf1 + " index: " + index1)
      println("compatible negative: " + newConf2 + " index: " + index2)

      // check compatibility
      assert(mapper.compatible(index1, index2) == false)
   }


   test("test map indices") {
      // set values corresponding to index value of 133
      val globalConf = Configuration(globalSet)
      val newConf = globalConf.setValues(Array(1, 2, 1, 1, 1))
      val globalIndex = newConf.computeIndex

      // creates configurations for partials
      val conf1 = Configuration(partial1)
      val newConf1 = conf1.setValues(Array(1, 2, 1, 1))
      val index1 = newConf1.computeIndex

      val conf2 = Configuration(partial2)
      val newConf2 =  conf2.setValues(Array(1, 1, 1))
      val index2 = newConf2.computeIndex

      // check map of conf to conf1 and conf2
      assert(mapper.mapIndices(globalIndex) == (index1, index2))
   }

   test("test map indices from operands to result") {
      // set values corresponding to index value of 129
      val globalConf = Configuration(globalSet)
      val newConf = globalConf.setValues(Array(1, 2, 1, 1, 1))

      val globalIndex = newConf.computeIndex

      // creates configurations for partials
      val conf1 = Configuration(partial1)
      val newConf1 = conf1.setValues(Array(1, 2, 1, 1))
      val index1 = newConf1.computeIndex

      val conf2 = Configuration(partial2)
      val newConf2 =  conf2.setValues(Array(1, 1, 1))
      val index2 = newConf2.computeIndex

      // check map of conf to conf1 and conf2
      assert(mapper.computeGlobalIndex(index1, index2) == globalIndex)
   }
}

