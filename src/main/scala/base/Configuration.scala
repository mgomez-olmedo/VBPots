package base

import potential.TreeStore
import utils.DataSizes

/**
  *  Class for representing a given assignment of values to a set of
  *  variables. A configuration for a VariableSet of X1(2 states),
  *  X2(3 states) and X3(4 states) allows the representation of all
  *  the possible assignments of these variables: from (0,0,0) to
  *  (1,2,3). For this domain the weights are (12, 4, 1). The last
  *  variable is the one with lower weight. Therefore values are
  *  immutable
  *
  * @constructor create an instance defined by a domain, values for
  *              the variables and an array of weights
  * @param domain VariableSet defining the domain of the configuration
  */
class Configuration(val domain:VariableSet,
                   val values:Vector[Int]) extends Iterable[Long]{
   /**
     * Set of weights for making computation of indices
     */
   val weights: Array[Long] = domain.weights

   /**
     * Iterator for the indices corresponding to the collection. With
     * this it is possible to make a loop over all the possible indices
     * of the configuration
     * @return iterator for possible indices
     */
   override def iterator: Iterator[Long] =
      (0 until domain.possibleValues.toInt).map(i => i.toLong).iterator

   /**
     * Creates a new configuration with the same set of variables
     * and a new set of values
     * @param newValues values to assign to the variables
     * @return new configuration with fixed values
     */
   def setValues(newValues: Array[Int]): Configuration = {
      // creates the new configuration
      Configuration(domain, newValues.toVector)
   }

   /**
     * computes the index corresponding to the values assigned
     * to the variables (according to variables weights).
     * @return computed index
     */
   def computeIndex: Long = {
      // for each variable (from 0 until domain.size (number of
      // variables), compute the product of value by weight.
      // Finally it computes the sum
      (0 until domain.size).map(index =>
                  values(index)*weights(index)).sum
   }

   /**
     * toString method
     * @return string containing the information of the object
     */
   override def toString: String = {
      val output1 = "Variables : "+domain+"\n" +
         "Weights : " + weights.mkString(" ")+"\n"
      val output2=output1 + "Values: " + values.mkString(" ") + "\n"
      output1 + output2
   }

   /**
     * Return the value for a given variable given an index
     * @param variable variable of interest
     * @param index index to use for computing the value of the variable
     * @return
     */
   def getVariableValue(variable: Variable, index: Long) : Int = {
      TreeStore.addGetValueCalls
      // get the index of the variable
      val indexOfVar: Int = domain.getIndex(variable)

      // Compute the value
      if(indexOfVar != -1)
         ((index/weights(indexOfVar)) %
            domain.cardinals(indexOfVar)).toInt
      else -1
   }

   /**
     * gets the value for a variable once the configuration
     * was computed
     * @param variable target variable
     * @return value of variable or -1 if not present
     */
   def getVariableValue(variable : Variable): Int = {
      // gets the index of the variable
      val indexOfVar : Int = domain.getIndex(variable)

      // gets the corresponding value if required
      if(indexOfVar != -1) values(indexOfVar) else -1
   }

   /**
     * Gets memory size for an object of variable type
     * @return memory size estimation
     */
   def getMemorySize : Long = {
      // gets size of domain
      val domainSize = domain.getMemorySize

      // gets sizes of values
      val valuesSize = values.length*DataSizes.INT

      // get sizes of weights
      val weightsSize = weights.length*DataSizes.LONG

      // return the sum
      domainSize + valuesSize + weightsSize
   }
}

/**
  * Companion object offering a factory method
  */
object Configuration{
   /**
     * Constructor
     * @param domain set of variables defining the domain of the
     *               configuration. Sets all the values to 0
     *               value
     * @return new instance of the class
     */
   def apply(domain: VariableSet):Configuration = {
      new Configuration(domain, List.fill(domain.getSize)(0).toVector)
   }

   /**
     * Constructor
     * @param domain set of variables defining the domain of the
     * configuration
     * @param values values for all the variables
     * @return new instance of the class
     */
   def apply(domain: VariableSet, values: Vector[Int]) : Configuration = {
      new Configuration(domain, values)
   }

   /**
    * Creates a new configuration with the values related to
    * a given index
    * @param domain domain of the configuration
    * @param index index to set
    * @return new configuration
    */
   def apply(domain : VariableSet, index : Long) : Configuration = {
      Configuration(domain,
                    Configuration.computeCoordinates(domain, index).toVector)
   }

   /**
    * project a set of values for a complete configuration into
    * a partial configuration, producing the array of coordinates
    * for partial configuration
    * @param complete complete domain of variables
    * @param refValues reference values for variables
    * @param partial partial domain to project on
    * @return new array of values
    */
   def projection(complete : VariableSet, refValues : Array[Int],
                  partial : VariableSet) : Array[Int] = {
      // creates result
      val newValues:Array[Int] = new Array[Int](partial.size)

      (0 until partial.getSize).foreach(index => newValues.update(index,
         refValues(complete.getIndex(partial.variableList(index)))))

      // return newValues
      newValues
   }

   /**
    * maps a certain set of values (for complete) into the index
    * respect to a partial configuration
    * @param complete base domain which values are passed as
    *                 argument
    * @param values values for variables in complete
    * @param partial domain of interest for the map
    * @return index in this configuration
    */
   def computeIndex(complete:VariableSet, values:Array[Int],
                    partial : VariableSet) : Long = {
      // project the configuration passed as argument on this
      val thisValues = Configuration.projection(complete, values, partial)

      // compute the index taking into account the values
      // of variables stored in newValues
      //(0 until partial.size).map(index =>
      //   thisValues(index)*partial.weights(index)).sum
      Configuration.computeIndex(partial, thisValues)
   }

   /**
    * public method for computing the coordinates corresponding
    * to a certain index
    * @param index index of interest. It will be used for computing
    *              the value of the variables
    * @return values of variables corresponding to the index
    *         passed as argument
    */
   def computeCoordinates(variables : VariableSet, index : Long): Array[Int] = {
      val newValues = new Array[Int](variables.size)

      // compute the value for each variable and update values
      (0 until variables.getSize).foreach(varIndex => {
         val coordinate =
            Configuration.computeVariableValue(variables, varIndex, index)
         newValues(varIndex) = coordinate
      })

      // return new values
      newValues
   }

   /**
    * Method for computing the value of a certain variable
    * (defined by its index) and according to a given index
    * (second argument)
    * @param varIndex index of the variable of interest
    * @param index index to use for computing the value of the
    *              variable of interest
    * @return value for the coordinate of interest and the corresponding
    *         index
    */
   def computeVariableValue(variables : VariableSet, varIndex: Int,
                            index: Long) : Int = {
      ((index/variables.weights(varIndex))%variables.cardinals(varIndex)).toInt
   }

   /**
    * computes the value for a certain variable in a domain and corresponding
    * to and index
    * @param variable target variable
    * @param domain domain
    * @param index index of interest
    * @return value of variable
    */
   def computeVariableValue(variable : Variable, domain : VariableSet,
                            index : Long) : Int = {
      // gets the index of the var
      val indexOfVar = domain.getIndex(variable)

      // compute the index
      ((index/domain.weights(indexOfVar))%domain.cardinals(indexOfVar)).toInt
   }

   /**
    * Auxiliary method for computing the index corresponding to
    * a certain set of values of the variables passed as argument
    * @param variables set of variables to consider
    * @param newValues values of variables for index computation
    * @return index for the values passed as argument
    */
   def computeIndex(variables : VariableSet, newValues : Array[Int]) : Long = {
      (0 until variables.size).map(index =>
         newValues(index)*variables.weights(index)).sum
   }
}
