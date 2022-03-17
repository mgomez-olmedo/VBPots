package potential

import base.Variable
import mapper.MarginalizeMapper
import potential.Operations.Marginalization
import potential.OperatorType.OperatorType
import utils.Util

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
    * Marginalization method
    *
    * @param valst    potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    */
   def marginalizeDefault(valst: ValueStore, variable: Variable): ValueStore = {
      // creates a mapper object for this operation
      val mapper = MarginalizeMapper(variable, valst.variables)

      // for each index in result gets the corresponding
      // values
      val content: Array[Double] =
      (0L until mapper.resultDomain.possibleValues).
         map(index => {
            Util.roundNumber(mapper.mapIndexFromResultToSource(index).
               map(valst.getValue).sum)
         }).toArray

      // creates the result as final step
      val result = ValueStore.createStore(valst.kind, mapper.resultDomain, content)

      // return result
      result
   }

}
