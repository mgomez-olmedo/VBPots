package potential

import base.Variable
import potential.Operations.Marginalization
import potential.OperatorType.OperatorType

import scala.collection.mutable

/**
 * Trait offering the functions required for implementing
 * marginalization strategies in a general way
 */
trait Marginalizer {
   /**
    * function used for the final operation
    */
   var marginalizationFunction : Operations.Marginalization = _

   /**
    * map of registered and available functions for combination
    */
   val marginalizationFunctions : mutable.Map[OperatorType, Operations.Marginalization] = mutable.Map()

   /**
    * returns the number of available functions for combination
    */
   def availableMarginalizationOptions: Int = marginalizationFunctions.size

   /**
    * selects the required function according to the parameter
    * @param opType identifier of the function to use
    */
   def setMarginalizer(opType : OperatorType): Unit = {
      // checks if the option is available
      if(marginalizationFunctions.contains(opType)){
         marginalizationFunction = marginalizationFunctions(opType)
      }
      else{
         // sets default operation
         marginalizationFunction = marginalizeDefault
      }
   }

   /**
    * stores a new function in the array of functions
    */
   def registerMarginalizationFunction(opType : OperatorType, newFunction : Operations.Marginalization): Option[Marginalization] = {
      marginalizationFunctions.put(opType, newFunction)
   }

   /**
    * default function with empty implementation
    * @param valst1 potential to marginalize
    * @param variable target variable
    * @return operation result
    */
   def marginalizeDefault(valst1 : ValueStore, variable : Variable) : ValueStore = {
      println(s"marginalization method ${OperatorType.DEFAULT} - Marginalizer trait")

      // shows an error message and return null
      println("This function should never be executed")
      null
   }
}
