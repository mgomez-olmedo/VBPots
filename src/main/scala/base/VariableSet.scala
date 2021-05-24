package base

import utils.{DataSizes, Serializator}

import scala.util.Random

/**
  * Class for storing list of variables acting as the domain of a
  * potential or configuration (for example)
  *
  * @constructor creates an instance of the class receiving as argument
  *              a list of variables
  * @param variableList    list of variables defining the set
  */
class VariableSet(val variableList : List[Variable])
                        extends Serializable with Iterable[Variable]{
   /**
     * Defines iterator operation
     * @return an iterator to loop over variables
     */
   override def iterator: Iterator[Variable] = variableList.iterator

   /**
     * Data member for storing variables in a map: index -> variable
     */
   val mapIndexVariable : Map[Int, Variable] = {
      require(variableList.distinct.length == variableList.length)
      variableList.indices.
         map(index => {(index, variableList(index))}).toMap
   }

   /**
     * Data member for storing variables in a map: variable -> index
     */
   val mapVariableIndex : Map[Variable, Int] = {
      variableList.indices.map(index => {
         (variableList(index), index)}
      ).toMap
   }

   /**
     * Weights of variables
     */
   val weights: Array[Long] = computeWeights

   /**
     * Number of values for each variable
     */
   val cardinals : Array[Int] = getCardinalities

   /**
     * Number of possible values for this set of variables
     */
   val possibleValues: Long = if(weights.length == 0) 0L
         else cardinals.map(_.toLong).product

   /**
     * Method for getting the variable name with a given index
     * @param index position of the variable in the domain (0 corresponds to
     *              the variable with bigger weight)
     * @return Option[bnet.Variable] (maybe index is not valid)
     */
   def getVariable(index : Int) : Variable = {
      mapIndexVariable.getOrElse(index, null)
   }

   /**
     * Gets a variable with a given name
     * @param name string with the name of a var
     * @return object of Variable class representing the variable with the name
     *         passed as argument
     */
   def getVariable(name : String) : Variable = {
      variableList.find(variable => variable.name == name).orNull
   }

   /**
     * Get the index of a certain variable in the domain
     * @param variable variable with the given position in the domain
     * @return index of the variable passed as argument
     */
   def getIndex(variable : Variable) : Int = {
      mapVariableIndex.getOrElse(variable, -1)
   }

   /**
     * Return the size of the set (the number of variables
     * contained in the domain)
     * @return number of variables in the domain
     */
   def getSize : Int = {
      mapIndexVariable.size
   }

   /**
     * Method to determine if a given variable belongs to the
     * domain
     * @param variable variable to look for in the domain
     * @return boolean flag (true/false)
     */
   def contains(variable : Variable) : Boolean = {
      variableList.contains(variable)
   }

   /**
     * Method for composing a String with variable information
     * @return string containing the information of the object
     */
   override def toString: String = {
      var output = "Variables in set:\n"
      variableList.indices.foreach(index => {
         val variable = getVariable(index)
         output += variable.name + " states: " + variable.getNumberStates
         output += " weight: " + weights(index) + "\n"

      })

      // return output
      output
   }

   /**
     * Simple,version of toString method
     * @return string with the information of the object
     */
   def simpleToString : String = {
      "vars in set: " + variableList.
            map(variable => variable.name).mkString(", ") + "(" +
               possibleValues + ")"
   }

   /**
     * Gets the union of variables in two different domains
     * @param other the domain to join with this
     * @return set of variables resulting of the union
     */
   def union(other : VariableSet) : VariableSet = {
      // Make the final VariableSet with the union of both
      // lists of variables
      new VariableSet(variableList.union(other.variableList).distinct)
   }

   /**
     * Gets the variables in both sets
     * @param other domain to intersect with this
     * @return domain with the variables both in this
     *         and other
     */
   def intersection(other : VariableSet) : VariableSet = {
      // return a variableSet with variables in common
      new VariableSet(variableList.intersect(other.variableList))
   }

   /**
     * Computes the difference between two domains
     * @param other domain to operate with
     * @return domain with the variables belonging to this
     *         and not in other
     */
   def difference(other : VariableSet) : VariableSet = {
      // make the final result
      new VariableSet(variableList.diff(other.variableList))
   }

   /**
     * Removes a variable from the set. The method returns a new
     * set after removing (if needed) the variable passed as
     * argument
     * @param variable variable to remove from the domain
     * @return VariableSet object with the result of the operation
     */
   def removeVariable(variable : Variable) : VariableSet = {
      // remove the variable passed as argument
      new VariableSet(variableList.diff(List(variable)))
   }

   /**
     * Computes the weights of the variables
     * Note: parallel method
     * @return array of weights for the domain
     */
   private def computeWeights: Array[Long] = {
      // Gets the number of states for each variable
      val states: Array[Int] = getCardinalities

      // now compute the weights: slice selects the variables
      // from the corresponding index to the right. states.indices
      // allows to define a Range. This sentence is equivalent to
      // 0 until states.length
      states.indices.map(index => {
         if (index == states.length-1) 1
         else {
            var weight=1L
            for(i <- index+1 until states.length){
               weight=weight*states(i)
            }
            weight
         }
      }).toArray
   }

   /**
     * Computes the weights of the variables in this set with
     * respect to the variables passed as argument
     * Note: parallel method
     * @param source set to use for computing weights
     * @return array with weights
     */
   def computeRelativeWeights(source : VariableSet) : Array[Long] = {
      // for each variable in the domain of this configuration
      val relativeWeights: Array[Long] = variableList.indices.map(index => {
         // Gets the var in this
         val variable = getVariable(index)

         // Gets the same variable in the domain of source
         val varIndexInSource = source.getIndex(variable)

         // If the index is -1, then return 0. Else, return the
         // corresponding weight source
         if (varIndexInSource == -1) 0
         else source.weights(varIndexInSource)
      }).toArray

      // return relative weights
      relativeWeights
   }

   /**
     * Produces an array with the number of states for each
     * variable
     * @return array with cardinalities
     */
   private def getCardinalities : Array[Int] = {
      // return the array of number states
      variableList.map(variable => variable.getNumberStates).toArray
   }

   /**
     * Gets memory size for an object of variable type
     * @note parallel method
     * @return memory size estimation of the object
     */
   def getMemorySize: Long = {
      (variableList.map(variable => variable.getMemorySize).sum +
                                 DataSizes.ARRAY).toLong
   }

   /**
    * computes the list of accumulated weights
    * @return
    */
   def getAccumulatedCardinals : List[Long] = {
      weights.indices.
         map(index => weights(index)*cardinals(index)).toList
   }
}

/**
  * Companion object for serialization purposes
  */
object VariableSet{
   /**
     * Apply method for object creation
     * @param variableList list of variables to include in the set
     * @return object created
     */
   def apply(variableList : List[Variable]): VariableSet = {
      new VariableSet(variableList)
   }

   /**
     * Serialization method for writing an object to a file
     * @param fileName name of file to create with serialized version
     * @param variableSet object to store
     */
   def writeObject(fileName : String, variableSet: VariableSet): Unit = {
      //val file=new FileOutputStream(fileName)
      //val stream=new ObjectOutputStream(file)
      //stream.writeObject(variableSet)
      Serializator.writeObject[VariableSet](fileName, variableSet)
   }

   /**
     * Serialization method for reading objects from files
     * @param fileName name of the file containing the serialized object
     * @return object with the information taken from the object
     */
   def readObject(fileName : String) : VariableSet = {
      //val file=new FileInputStream(fileName)
      //val stream=new ObjectInputStream(file)
      //stream.readObject().asInstanceOf[VariableSet]
      Serializator.readObject[VariableSet](fileName)
   }

   /**
     * Generates a set of variables of a certain size and having
     * maxCardinality as limit for their number of states
     * @param numberVariables number of variables to generate
     * @param maxCardinality maximum cardinality for variables
     * @return object with the variables randomly created
     */
   def generateRandomSet(numberVariables : Int,
                         maxCardinality : Int):VariableSet = {
      // generates the required number of variables
      val varList = for(i <- 1 to numberVariables)
         yield Variable.generateWithRandomCardinality(i, maxCardinality)

      // now create the variable set
      VariableSet(varList.toList)
   }

   /**
     * Generates a set of variables with the following constraints:
     * all variables are limited to a certain cardinality and the
     * global cardinality contained in a given interval
     * maxGlobalCardinality must be big enough to ensure the
     * correct behaviour of the method (it should be at least
     * the multiplication of minGlobalCardinality * maxCardinality)
     * @param maxCardinality max cardinality for variables
     * @param minGlobalCardinality global maximum cardinality
     * @param maxGlobalCardinality global minimum cardinality
     * @return VariableSet object created
     */
   def generateRandomSet(maxCardinality:Int,
                         minGlobalCardinality : Long,
                         maxGlobalCardinality : Long):VariableSet = {
      // generates a random set of variables for 20 variables and the
      // number of states given by maxCardinality. This allows to make
      // a second phase of selection until reaching the desired global
      // cardinality
      val numberVariables = 50
      var stop:Boolean = false
      var globalSet : VariableSet = null
      var firstIndexValue : Int = -1

      while(! stop) {
         // generate a random set of variables
         globalSet = generateRandomSet(numberVariables, maxCardinality)

         // compose from weights the global cardinality multiplying
         // by the cardinality of the last variable
         val accumulatedCardinals = globalSet.getAccumulatedCardinals

         // check the index of the first variable holding the condition
         // of max and min global cardinality
         val firstIndex = (globalSet.size - 1 to 0 by -1).find(index => {
            accumulatedCardinals(index) >= minGlobalCardinality &&
               accumulatedCardinals(index) <= maxGlobalCardinality
         })

         // check if the generation fails
         if(firstIndex.isDefined){
            stop = true
            firstIndexValue=firstIndex.get
         }
         else{
            firstIndexValue = -1
         }
      }

      // now keep the final global set selecting the variables
      // from firstIndex to the end
      val finalSet = globalSet.variableList.
         takeRight(globalSet.size - firstIndexValue)

      // return the set
      new VariableSet(finalSet)
   }

   /**
    * build a new variable set from the one passed as first argument
    * and randomly selecting the number of variables given by size
    * @param set variable set to use
    * @param size number of variables to select
    * @return
    */
   def getRandomSelection(set : VariableSet, size : Int) : VariableSet = {
      // shuffle the list of variables and selects some of them (size)
      val list = Random.shuffle(set.variableList)

      // make a VariableSet with selected variables
      VariableSet(list.take(size))
   }
}
