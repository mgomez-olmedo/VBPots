package potential

import potential.Operations.Combination
import potential.OperatorType.OperatorType

import scala.collection.mutable

/**
 * Trait offering the functions required for implementing
 * combination strategies in a general way
 */
trait Combiner {
   /**
    * function to use for combination: initially default
    * function is employed
    */
   var combinationFunction : Operations.Combination = _

   /**
    * map of registered and available functions for combination
    */
   val combinationFunctions : mutable.Map[OperatorType, Operations.Combination] = mutable.Map()

   /**
    * returns the number of available functions for combination
    */
   def availableCombinationFunctions: Int = combinationFunctions.size

   /**
    * selects the required function according to the parameter
    * @param opType identifier of the function to use
    */
   def setCombiner(opType : OperatorType): Unit = {
      // checks if the option is available
      if(combinationFunctions.contains(opType)){
         combinationFunction = combinationFunctions(opType)
      }
      else{
         // sets default operation
         combinationFunction = combineDefault
      }
   }

   /**
    * stores a new function in the array of functions
    */
   def registerCombinationFunction(opType : OperatorType, newFunction : Operations.Combination): Option[Combination] = {
      combinationFunctions.put(opType, newFunction)
   }

   /**
    * default implementation
    * @param valst1 first operator
    * @param valst2 second operator
    * @return result of combination
    */
   def combineDefault(valst1 : ValueStore, valst2 : ValueStore) : ValueStore = {
      println(s"combine alternative ${OperatorType.DEFAULT} - Combiner trait")

      // shows an error message and return null
      println("This function should never be executed")
      null
   }
}
