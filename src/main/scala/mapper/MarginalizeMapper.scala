package mapper

import base.VariableSet
import base.Variable

/**
 * Class for making the mapping between indices and coordinates
 * in marginalization operations
 *
 * @constructor class constructor
 * @param variable variable to remove from the domain passed as
 *                 second argument
 * @param source   domain of variables to be used as source for the operation
 */
class MarginalizeMapper(val variable: Variable, val source: VariableSet) {
   /**
    * domain of the result
    */
   val resultDomain: VariableSet = source.removeVariable(variable)

   /**
    * position of the removed variable into source
    */
   val indexOfVariableToRemove: Int = source.getIndex(variable)

   /**
    * the number of states for source variables
    */
   val sourceDomainCardinalities: Array[Int] = source.cardinals

   /**
    * number of states for result domain variables
    */
   val resultDomainCardinalities: Array[Int] = resultDomain.cardinals

   /**
    * weights of the source variables
    */
   val sourceDomainWeights: Array[Long] = source.weights

   /**
    * weights of the final potential domain
    */
   val resultDomainWeights: Array[Long] = resultDomain.weights

   /**
    * indices of the variables in result with respect to
    * source domain
    */
   val indicesOfResultDomainVarsIntoSource: Array[Int] =
                              getIndicesOfResultDomainVarsIntoSource

   /**
    * max number for index in source
    */
   val maxIndexSourceDomain: Long = source.possibleValues

   /**
    * max number for index in result
    */
   val maxIndexResultDomain: Long =
                  resultDomainWeights(0) * resultDomainCardinalities(0)

   /**
    * Data structure for storing the values of the variables
    * in operations of indices computation
    */
   val variableValues: Array[Int] = Array.fill[Int](source.size)(0)

   /**
    * define the indices that variables belonging in result domain
    * present in source domain
    *
    * @return array of indices
    */
   def getIndicesOfResultDomainVarsIntoSource: Array[Int] = {
      resultDomain.variableList.map(variable =>
                     source.getIndex(variable)).toArray
   }

   /**
    * map a given index of the source domain into the result one
    *
    * @param sourceIndex index to map
    * @return index related to argument but defined on result
    *         domain
    */
   def mapIndexFromSourceToResult(sourceIndex: Long): Long = {
      // Gets the coordinates for each variable
      val coordinates: Array[Int] =
         computeCoordinates(source, sourceIndex)

      // Gets the coordinates in result
      val coordinatesInResult =
         coordinates.indices.map(index => {
            if (index == indexOfVariableToRemove) -1
            else coordinates(index)
         }).filter(value => value != -1).toArray

      // compute the index
      computeIndex(resultDomainWeights, coordinatesInResult)
   }

   /**
    * given a index on result return all the indices that are
    * compatible in source
    *
    * @param index index to consider
    * @return list of compatible indices in source domain
    * @note TODO - complete review of this method is required
    */
   def mapIndexFromResultToSource(index: Long): List[Long] = {

      // compute the coordinates corresponding to index
      // val coordinatesInResult: Array[Int] = computeCoordinates(resultDomain, index)
      val resultValues = storeCoordinates(resultDomain, index)

      // computes the initial index
      val initialIndex = computeIndex(sourceDomainWeights, resultValues)

      // compute the whole list of indices from initialIndex
      val indices = (0 until variable.getNumberStates).
         map(state => initialIndex + sourceDomainWeights(indexOfVariableToRemove) * state).toList

      // return indices
      indices
   }

   /**
    * Stores the coordinates in variableValues data member
    *
    * @param domain variables composing the domain
    * @param index target index
    */
   private def storeCoordinates(domain: VariableSet, index: Long) = {
      val values=new Array[Int](source.size)
      domain.foreach(variable => {
         values(source.getIndex(variable)) =
            computeCoordinateForVarInDomain(domain, variable, index)
      })

      // return values
      values
   }

   /**
    * private method for computing the coordinates corresponding to a
    * certain index
    *
    * @param domain set of variables to consider
    * @param index  index defined on domain
    * @return array of coordinates related to index
    * NOTE: this method can not be parallelized: the
    * indexes of coordinates must be kept
    */
   private def computeCoordinates(domain: VariableSet,
                                  index: Long): Array[Int] = {
      val values=new Array[Int](domain.size)
      domain.foreach(variable => {
         values(domain.getIndex(variable))=
            computeCoordinateForVarInDomain(domain, variable, index)
      })

      // return values
      values
   }

   /**
    * computes the coordinate for a given var in a domain and
    * related to a certain index
    *
    * @param domain   set of variables to consider
    * @param variable target variable
    * @param index    index to analyze
    * @return value for variable related to index
    */
   private def computeCoordinateForVarInDomain(domain: VariableSet,
                     variable: Variable, index: Long): Int = {
      // get the index of the variable into the domain
      val varIndex = domain.getIndex(variable)

      // now gets the value of the variable given the index
      // passed as last argument and the weights
      val quotient: Int = (index / domain.weights(varIndex)).toInt

      if (varIndex == 0) {
         // just divide by the corresponding result weight
         quotient
      }
      else {
         // Divide quotient by the number of states for
         // variable in varIndex and return the remainder
         quotient % domain.cardinals(varIndex)
      }
   }

   /**
    * private method for computing the index of a coordinate in
    * source into the result domain
    *
    * @param weights     vector of weights for variables
    * @param coordinates coordinates to analyze and produce an index
    * @return resultant index
    */
   private def computeIndex(weights: Array[Long],
                            coordinates: Array[Int]): Long = {
      weights.indices.map(index =>
         weights(index) * coordinates(index)).sum
   }

   /**
    * Checks if two indices in the source domain are compatible,
    * that is, the only difference between them is related to
    * the variable to remove. The difference between indices
    * must be a multiple of the var to remove weight
    *
    * @param index1 first index to check
    * @param index2 second index to check
    * @return result of check
    */
   def compatible(index1: Long, index2: Long): Boolean = {
      // gets the value of variable
      val valueOfVar = computeCoordinateForVarInDomain(source, variable, index1)

      // compute the array of differences related to the
      // target variable
      val possibleDifferences =
         (0 until variable.getNumberStates).
            map(value => valueOfVar - value).
            map(_ * sourceDomainWeights(indexOfVariableToRemove)).toList

      // check if the difference between indexes passed as
      // argument is contained in the set of computed differences
      possibleDifferences.contains(index1-index2)
   }

   /**
    * gets the list of compatible indexes to the index passed
    * as argument
    * @param index target index
    * @return list of compatible indexes
    */
   def getCompatibles(index : Long) : List[Long] = {
      // gets the value of variable
      val valueOfVar = computeCoordinateForVarInDomain(source, variable, index)

      // compute baseIndex removing the value of variable
      val base = index - valueOfVar*sourceDomainWeights(indexOfVariableToRemove)

      (0 until variable.getNumberStates).
         map(base + _*sourceDomainWeights(indexOfVariableToRemove)).toList
   }
}

/**
 * Companion object
 */
object MarginalizeMapper {
   /**
    * apply method acting as factory method
    * @constructor creates a new object from arguments
    * @param variable  target variable
    * @param variables domain variables
    * @return created object
    */
   def apply(variable: Variable, variables: VariableSet):
                                          MarginalizeMapper = {
      // Creates combiner object
      new MarginalizeMapper(variable, variables)
   }
}
