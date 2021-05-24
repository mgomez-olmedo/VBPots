package experiments

import base.{Variable, VariableSet}
import potential.{Potential, TableStore, ValueStoreTypes}

/**
  * Object for making a single test of a concrete combination
  * of potentials. It extends App for being executable
  */
object SingleOperation extends App{
   /**
     * X1 variable definition: 3 states
     */
   val variable1 = new Variable("X1", List("x11", "x12", "x13"))

   /**
     * X2 variable definition: 2 states
     */
   val variable2 = new Variable("X2", List("x21", "x22"))

   /**
     * X3 variable definition: 3 states
     */
   val variable3 = new Variable("X3", List("x31", "x32", "x33"))

   /**
     * First domain: (X2, X1)
     */
   val domain1 = new VariableSet(List(variable2, variable1))

   /**
     * Second domain: (X3, X1)
     */
   val domain2 = new VariableSet(List(variable3, variable1))

   /**
     * Definition of values for the first potential
     */
   val valuesPot1 = Array(0.1, 0.6, 0.3, 0.3, 0.3, 0.4);

   /**
     * Definition of values of the second potential
     */
   val valuesPot2 = Array(0, 0, 1, 1, 0, 0, 0, 0.5, 0.5)

   /**
     * Creates the first CPT
     */
   val store1Table = TableStore(domain1, valuesPot1)

   /**
     * Creates second CPT
     */
   val store2Table = TableStore(domain2, valuesPot2)

   /**
     * Creates the first potential
     */
   val potential1 = Potential(store1Table)

   /**
     * Creates the second potential
     */
   val potential2 = Potential(store2Table)

   /**
     * Perform potential combination
     */
   val potCombination = potential1.combine(potential2)

   /**
     * Makes conversion to the desired types
     */
   val target = ValueStoreTypes.IDPMUT

   /**
     * Perform conversion of first potential
     */
   val potential1AIM = potential1.convert(target)

   /**
     * Perform conversion of the second potential
     */
   val potential2AIM = potential2.convert(target)

   /**
     * Shows first potential
     */
   println(potential1AIM)

   /**
     * Shows second potential
     */
   println(potential2AIM)

   /**
     * Combine with the new type
     */
   val potCombinationAIM = potential1AIM.combine(potential2AIM)

   /**
     * Shows the result
     */
   println("\n Combination: ")
   println(potCombinationAIM)

   /**
     * Makes a marginalization on the result of the combination
     */
   val potMarginalizationAIM = potCombinationAIM.marginalize(variable1)

   /**
     * Shows the result
     */
   println("Marginalization of combination: ")
   println(potMarginalizationAIM)
}
