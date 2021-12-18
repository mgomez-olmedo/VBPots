package mapper

import base.{Configuration, Variable, VariableSet}

/**
 * Class for making the mapping of indices from result to
 * operands in combination operations
 * @constructor creates a new object using two domains as
 *              operands
 * @param operand1 domain of first potential
 * @param operand2 domain of second potential
 */
class CombineMapper(val operand1: VariableSet, val operand2: VariableSet) {
   /**
    * Stores the domain of the resultant set
    */
   val resultDomain: VariableSet = operand1.union(operand2)

   /**
    * Stores the weights of the result domain obtained after
    * combining operand1 and operand2 domains
    */
   val resultDomainWeights: Array[Long] = resultDomain.weights

   /**
    * Stores the info for each variable: cardinality, weight
    * in source and dest and value
    */
   val mapperInfo: Map[Variable, VariableInfo] = createInfo

   /**
    * Stores weights of variables in operand1 and operand2
    */
   val weights: (Array[Long], Array[Long]) = computeWeights

   /**
    * Stores a configuration for computing indexes
    */
   val resultConfiguration: Configuration = Configuration(resultDomain)

   /**
    * Stores the common variables
    */
   val commonVariables: VariableSet = operand1.intersection(operand2)

   /**
    * Stores exclusive variables in source and dest
    * TODO: check if this is needed
    */
   val sourceExclusiveVars: List[Variable] =
               operand1.difference(operand2).variableList
   val destExclusiveVars: List[Variable] =
               operand2.difference(operand1).variableList

   /**
    * Stores the number os states for each variable
    */
   val resultDomainCardinalities: Array[Int] =
               resultDomain.cardinals

   /**
    * Stores the max number index into result
    */
   val resultDomainMaxIndex: Long =
               resultDomainWeights(0) * resultDomainCardinalities(0)

   /**
    * checks if two indexes in operands are compatible
    * @param index1 index in operand1
    * @param index2 index in operand2
    * @return boolean flag
    */
   def compatible(index1 : Long, index2 : Long) : Boolean = {
      val result = commonVariables.find(variable => {
         Configuration.computeVariableValue(variable, operand1, index1) !=
           Configuration.computeVariableValue(variable, operand2, index2)
      })

      // if result is empty, all the common variables are compatible
      result.isEmpty
   }

   /**
    * Computes the global index from the indexes in both potential
    * operands
    * @param index1 index in first potential
    * @param index2 index in second potential
    * @return index in result potential
    */
   def computeGlobalIndex(index1 : Long, index2 : Long ) : Long = {

      // considers each variable in result
      val values = resultDomain.map(variable => {
         if(operand1.getIndex(variable) != -1)
            Configuration.computeVariableValue(variable, operand1, index1)
         else Configuration.computeVariableValue(variable, operand2, index2)
      }).toArray

      // computes the index
      Configuration.computeIndex(resultDomain, values)
   }

   /**
    * maps the index passed as argument to the configurations
    * represented with operand1 and operand2
    *
    * @param index index to map
    * @return tuple with (index in conf1, index in conf2)
    */
   def mapIndices(index : Long) : (Long, Long) = {
      // compute the coordinates for index
      val values = Configuration.computeCoordinates(resultDomain, index)

      //val index1 = weights._1.zip(values).map(pair => pair._1*pair._2).sum
      //val index2 = weights._2.zip(values).map(pair => pair._1*pair._2).sum

      var index1 : Long = 0L
      var index2 : Long = 0L
      (0 until resultDomain.size).foreach(index => {
         index1 = index1 + values(index)*weights._1(index)
         index2 = index2 + values(index)*weights._2(index)
      })

      // return index1 and index2
      (index1, index2)
   }


   /**
    * maps the index passed as argument to the configurations
    * represented with operand1 and operand2
    *
    * @param index index to map
    * @return tuple with (index in conf1, index in conf2)
    */
   def mapIndices1(index : Long) : (Long, Long) = {
      // compute the coordinates for index
      val values = Configuration.computeCoordinates(resultDomain, index)

      // compute the indices in conf1 and conf2 from this
      (Configuration.computeIndex(resultDomain, values, operand1),
        Configuration.computeIndex(resultDomain, values, operand2))
   }

   /**
    * Map a given index of the final potential into both
    * operands
    *
    * @param index target index
    * @return tuple with indices corresponding to index for
    *         operand1 and operand2
    */
   def mapIndices2(index: Long): (Long, Long) = {
      // Gets the coordinates for each variable
      val coordinates = Configuration.computeCoordinates(resultDomain, index)

      // project values for both operands
      val coord1 = Configuration.projection(resultDomain, coordinates, operand1)
      val coord2 = Configuration.projection(resultDomain, coordinates, operand2)

      // Gets the indices in both operands
      (Configuration.computeIndex(operand1, coord1),
         Configuration.computeIndex(operand2, coord2))
   }

   /**
    * Map a given index of the final potential into both
    * operands
    *
    * @param index target index
    * @return tuple with indices corresponding to index for
    *         operand1 and operand2
    */
   def mapIndices3(index: Long): (Long, Long) = {
      // Gets the coordinates for each variable
      val coordinates = Configuration.computeCoordinates(resultDomain, index)

      // sets values to mapper info
      coordinates.indices.
         foreach(index =>
            mapperInfo.getOrElse(resultDomain.getVariable(index), null).
               variableValue=coordinates(index))

      // computes both indexes
      var index1=0L
      operand1.foreach(variable =>{
         val varInfo = mapperInfo.getOrElse(variable, null)
         index1 = index1 + varInfo.variableValue*varInfo.weightInOperand1
      })

      var index2 = 0L
      operand2.foreach(variable =>{
         val varInfo = mapperInfo.getOrElse(variable, null)
         index2 = index2 + varInfo.variableValue*varInfo.weightInOperand2
      })

      // compose the pair
      (index1, index2)
   }

   /**
    * maps the index passed as argument to the first operand potential
    *
    * @param index index to map
    * @return index in operand1
    */
   def mapToOperand1(index : Long) : Long = {
      // compute the coordinates for index
      val values = Configuration.computeCoordinates(resultDomain, index)

      // compute the indices in conf1 and conf2 from this
      Configuration.computeIndex(resultDomain, values, operand1)
   }

   /**
    * maps the index passed as argument to the second operand potential
    *
    * @param index index to map
    * @return index in operand2
    */
   def mapToOperand2(index : Long) : Long = {
      // compute the coordinates for index
      val values = Configuration.computeCoordinates(resultDomain, index)

      // compute the indices in conf1 and conf2 from this
      Configuration.computeIndex(resultDomain, values, operand2)
   }

   /**
    * Private method for creating the map with the info
    * for each variable*
    * @return dictionary with entries of Variable (key)
    *         and VariableInfo (value)
    */
   private def createInfo : Map[Variable, VariableInfo] = {
      // maps variables in potentials
      resultDomain.variableList.map(variable => {
         val indexInPot1: Int =
            operand1.mapVariableIndex.getOrElse(variable, -1)
         val indexInPot2 =
            operand2.mapVariableIndex.getOrElse(variable, -1)
         // gets the index in result: sure this variable will be
         // presented in result domain
         val indexInResult =
            resultDomain.mapVariableIndex.getOrElse(variable, -1)

         // sets 1 as weight if the variable is not present in source
         // or dest or the proper value (a 1 value does not modify weight
         // computation)
         val weightInPot1 = if (indexInPot1 == -1) 0
                     else operand1.weights(indexInPot1)
         val weightInPot2 = if (indexInPot2 == -1) 0
                     else operand2.weights(indexInPot2)
         val weightInResult = resultDomain.weights(indexInResult)

         // creates the VariableInfo object
         (variable, new VariableInfo(resultDomain.cardinals(indexInResult),
                     weightInPot1, weightInPot2, weightInResult, 0))
      }).toMap
   }

   /**
    * private method for computing the weights of variables in operand
    * potentials in order to speedup posterior indexes computation
    * @return
    */
   private def computeWeights : (Array[Long], Array[Long]) = {
      val weights1 = new Array[Long](resultDomain.size)
      val weights2 = new Array[Long](resultDomain.size)

      // considers each variable in mapperInfo
      (0 until resultDomain.variableList.size).foreach(index => {
         weights1(index) = mapperInfo.get(resultDomain.variableList(index)).get.weightInOperand1
         weights2(index) = mapperInfo.get(resultDomain.variableList(index)).get.weightInOperand2
      })

      // return arrays of weights
      (weights1, weights2)
   }


   /**
    * Private class for storing info about the variables
    * @constructor
    * @param cardinality      cardinality of the variable
    * @param weightInOperand1 weight of the variable in operand1
    *                         potential
    * @param weightInOperand2 weight of the variable in operand2
    *                         potential
    * @param weightInResult   weight of the variable in the result
    *                         potential
    */
   class VariableInfo(val cardinality: Int,
                      val weightInOperand1: Long,
                      val weightInOperand2: Long,
                      val weightInResult: Long,
                      var variableValue:Int) {
   }
}

/**
 * Companion object offering factory methods
 */
object CombineMapper {
   /**
    * apply method for creating objects
    * @param variableSet1 domain of first potential
    * @param variableSet2 domain of second potential
    * @return created object
    */
   def apply(variableSet1: VariableSet, variableSet2: VariableSet):
                                                      CombineMapper = {
      // Gets the potential with bigger domain
      val variables = if (variableSet1.getSize >= variableSet2.getSize) {
         (variableSet1, variableSet2)
      }
      else {
         (variableSet2, variableSet1)
      }

      //new CombineMapper(variables._1, variables._2)
      new CombineMapper(variables._1, variables._2)
   }
}
