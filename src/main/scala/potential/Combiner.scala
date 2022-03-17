package potential

import mapper.CombineMapper
import potential.Operations.Combination
import potential.OperatorType.OperatorType
import utils.Util

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
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   def combineDefault(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // considers all the indexes of result and produces a list
      // of pairs type (double - long) storing the values related
      // to each index in result
      val content = (0.toLong until mapper.resultDomain.possibleValues).
         map(index => {
            // produces the pair
            val indexes: (Long, Long) = mapper.mapIndices2(index)
            //println("index in result: " + index + " pairs of indexes: " + indexes)

            // produces the value
            Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2))
         }).toArray

      // creates the result
      val result = ValueStore.createStore(valst1.kind, mapper.resultDomain, content)

      // return result
      result
   }

}
