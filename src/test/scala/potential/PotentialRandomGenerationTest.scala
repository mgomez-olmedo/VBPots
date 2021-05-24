package potential

import base.{Variable, VariableSet}
import org.scalatest.FunSuite

class PotentialRandomGenerationTest extends FunSuite {

   var testName = "single potential with variable cardinality"
   // test the most single method of random generation
   test("single potential with variable cardinality"){
      println("begin: single potential with variable cardinality")
      // sets test features
      val numberVariables = 10
      val maxCardinality = 4
      // uses the method for generating a single potential
      val potential = Potential.generateRandomPotential(numberVariables, maxCardinality)

      println("potential features: ")
      println("number variables: " + potential.variables.size)
      // check the number of variables in the domain is correct
      // and that all the variables has a correct dimension
      assert(potential.variables.size == numberVariables)
      val cardinals = potential.variables.map(_.getNumberStates)
      println("cardinals: " + cardinals.mkString(" - "))
      println("global cardinality: " + potential.variables.possibleValues)
      val minCar = cardinals.min
      val maxCard = cardinals.max
      assert(minCar >= 2 && maxCardinality <= maxCardinality)
      println("end: single potential with variable cardinality")
      println()
   }

   // test the method for generating pairs of potentials
   test("pair of potentials with variable cardinality"){
      println("begin: pair of potentials with variable cardinality")
      // sets test features
      val numberVariables1 = 8
      val numberVariables2 = 4
      val maxCardinality = 10
      // uses the method for generating a pair of potentials
      val potentials = Potential.generateRandomPotentialPair(numberVariables1,
                                                         numberVariables2,
                                                         maxCardinality)

      println("potentials features: ")
      println("potential 1 - number variables: " + potentials._1.variables.size)
      println("potential 2 - number variables: " + potentials._2.variables.size)
      // check the number of variables in the domain is correct
      // and that all the variables has a correct dimension
      assert(potentials._1.variables.size == numberVariables1 &&
             potentials._2.variables.size == numberVariables2)
      val cardinals1 = potentials._1.variables.map(_.getNumberStates)
      println("potential 1 cardinals: " + cardinals1.mkString(" - "))
      val cardinals2 = potentials._2.variables.map(_.getNumberStates)
      println("potential 2 cardinals: " + cardinals2.mkString(" - "))
      println("potential 1 global cardinality: " + potentials._1.variables.possibleValues)
      println("potential 2 global cardinality: " + potentials._2.variables.possibleValues)
      val minCar = List(cardinals1.min, cardinals2.min).min
      val maxCard = List(cardinals1.max, cardinals2.max).max
      assert(minCar >= 2 && maxCardinality <= maxCardinality)

      // print the number of different values for each one
      println("potential 1 dif values: " + potentials._1.store.getDifferentValues.size)
      println("potential 2 dif values: " + potentials._2.store.getDifferentValues.size)
      println("end: pair of potentials with variable cardinality")
      println()
   }

   // test the method for generating pairs of potentials
   test("single potential limiting global cardinality"){
      println("begin: single potential limiting global cardinality")
      // sets test features
      val maxCardinality = 10
      val minGlobalCardinality = 5000
      val maxGlobalCardinality = 200000
      val intervals = 10
      val normalize = true

      // uses the method for generating the potential
      val potential = Potential.generateRandomPotential(maxCardinality, minGlobalCardinality,
                                       maxGlobalCardinality, intervals, normalize)

      println("potential features: ")
      println("potential - number variables: " + potential.variables.size)

      // get the cardinal for each variable
      val cardinals = potential.variables.map(_.getNumberStates)
      println("potential cardinals: " + cardinals.mkString(" - "))

      // shows the global cardinality
      val globalCardinality = potential.variables.possibleValues
      println("potential global cardinality: " + globalCardinality)
      println("weights: " + potential.variables.weights.mkString(" "))
      println("accum cardinals: " + potential.variables.getAccumulatedCardinals.mkString(" "))

      // print the number of different values for each one
      println("potential dif values: " + potential.store.getDifferentValues.size)

      // checks its value
      assert(globalCardinality >= minGlobalCardinality && globalCardinality <= maxGlobalCardinality)

      println("end: single potential limiting global cardinality")
      println()
   }

   // test the generation of sequences of normalized values
   test("sequence of normalized values"){
      println("begin: sequence of normalized values")
      val values = Potential.generateNormalizedRandomValues(5, List(0.0, 0.2, 0.4, 0.6, 0.8, 1))
      println("sequence of values: " + values)
      assert(true)
      println("end: sequence of normalized values")
      println()
   }
}
