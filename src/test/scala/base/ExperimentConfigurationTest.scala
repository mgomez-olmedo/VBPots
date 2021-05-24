package base

import org.scalatest.FunSuite

/**
 * Class for testing the methods of Configuration class. The
 * test is based on defining a domain with variables X1 (2
 * states), X2 (3 states), X3 (4 states), X4 (2 states) and
 * X5 (3 states). This domain produces the following vector
 * of weights: 72, 24, 6, 3, 1
 * The last variable is the one with lower weight
 */
class ExperimentConfigurationTest extends FunSuite {
   // Create variables
   /**
    * Set of variables defining the configuration:
    * X1, X2, X3, X4, X5
    */
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 =
      new Variable("X3", List("x31", "x32", "x33", "x34"))
   val variable4 = new Variable("X4", List("x41", "x42"))
   val variable5 = new Variable("X5", List("x51", "x52", "x53"))

   // Weights vector
   // 72 24 6 3 1

   /**
    * the variables defining the configuration are stored as
    * a object of VariableSet class
    */
   val variableSet = new VariableSet(List(variable1, variable2,
               variable3, variable4, variable5))

   /**
    * creates a configuration for this set of variables
    */
   val configuration: Configuration = Configuration(variableSet)

   /**
    * Tests the computation of the weights
    */
   test("Computation of weights") {
      assert(configuration.weights(0) === 72)
      assert(configuration.weights(1) === 24)
      assert(configuration.weights(2) === 6)
      assert(configuration.weights(3) === 3)
      assert(configuration.weights(4) === 1)
   }

   /**
    * Test the creation of the iterator from the configuration
    */
   test("Iteration with iterator") {
      // use iterator to loop over all the possible indices and check the
      // last one is equals to the number of possible values according to
      // the domain of the configuration
      val iterator = configuration.iterator
      val max = iterator.reduceLeft((a, b) => if (a > b) a else b)
      assert(max == configuration.domain.possibleValues - 1)
   }

   /**
    * Test the result of setting the max value for each variable
    */
   test("set values") {
      // set values and store the ofsset
      val offset = configuration.setValues(Array(1, 2, 3, 1, 2))

      // the offset of the configuration with max values for all the
      // variables will be the same as the number of possible values
      // if the domain mninus 1
      assert(offset.computeIndex == configuration.domain.possibleValues - 1)
   }

   test("set values from a given configuration and compute index") {
      val conf2 = Configuration(variableSet)

      // sets a certain combination of values
      val conf2Modified = conf2.setValues(Array(1, 0, 3, 0, 0))

      // perform the operation of computing the index
      assert(conf2Modified.computeIndex == 90)
   }

   /**
    * test the method converting the coordinates into indices
    */
   test("test from values to offset") {
      // check the conversion of the coordinates with max value for
      // each variable to index
      val newConf = configuration.setValues(Array(1, 2, 3, 1, 2))
      assert(newConf.computeIndex == configuration.domain.possibleValues - 1)
   }

   /**
    * test the values related to variables given a certain index
    */
   test("test get variable value") {
      val maxIndex = configuration.domain.possibleValues - 1

      // check the values related to the max. index
      assert(configuration.getVariableValue(variable1, maxIndex) == 1)
      assert(configuration.getVariableValue(variable2, maxIndex) == 2)
      assert(configuration.getVariableValue(variable3, maxIndex) == 3)
      assert(configuration.getVariableValue(variable4, maxIndex) == 1)
      assert(configuration.getVariableValue(variable5, maxIndex) == 2)
   }
}

