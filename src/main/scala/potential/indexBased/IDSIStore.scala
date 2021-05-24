package potential.indexBased

import base.{Variable, VariableSet}
import potential.{ValueStoreTypes, _}
import utils.{DataSizes, Util}

import scala.collection.Set

/**
 * Class for storing values with the following structure:
 * values are stored in an array; each value is paired with
 * the same position in another array where indices are
 * stored in sets. In this case immutable arrays are employed
 * @constructor creates a new instance using the data
 *              passed as argument
 * @param variables domain of the related potential
 * @param indicesSets array of sets of indices
 * @param values array of values
 */
case class IDSIStore(variables: VariableSet,
                     indicesSets: Array[Set[Long]],
                     values: Array[Double]) extends ValueDrivenStore
                         with Combiner with Marginalizer{

   /**
    * default value behaviour
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.IDSETIMMUT

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value related to the index passed as argument
    */
   override def getValue(index: Long): Double = {
      // find the index in the sets
      val indexResult = indicesSets.indices.
         find(indexInArray => indicesSets(indexInArray).
            contains(index)).getOrElse(-1)

      // return the value stored in the corresponding index
      val result = if (indexResult != -1) values(indexResult)
                  else Util.DEFAULTVALUE

      // return result
      result
   }

   /**
    * adds a value to the store. As the collection of values and
    * sets of indices are immutable, a new object is created
    *
    * @param value new value to store
    * @param index index corresponding to the value
    */
   def addValueForRepresentation(value: Double, index: Long): IDSIStore = {
      // checks if the value is already contained into
      // values
      val indexForValue =
         values.indices.par.
            find(index => values(index) == value).getOrElse(-1)

      // if the value is already defined, add a new entry to
      // indices. In any other case it is needed to add a
      // new value
      val result =
         if (indexForValue != -1) {
            indicesSets.update(indexForValue,
                              indicesSets(indexForValue) + index)
         this
      }
      else {
         // creates a new set for the the index and a new value and
         // return a new object
         val newSet: Set[Long] = Set[Long]() + index
         val newIndicesSets = indicesSets :+ newSet
         val newValues: Array[Double] = values :+ value
         IDSIStore(variables, newIndicesSets, newValues)
      }

      // return result
      result
   }

   /**
    * splits the store into a collection of stores for
    * a single value each one
    *  @return
    */
   override def split: List[ValueStore] = {
      values.indices.map(index => {
         IDSIStore(variables, values(index), indicesSets(index))
      }).toList
   }

   /**
    * Gets the complete list of values
    *
    * @return complete list of values
    */
   override def getListValues: List[Double] = {
      (0L until variables.possibleValues).
         map(index => getValue(index)).toList
   }

   /**
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues: List[Double] = {
      Util.DEFAULTVALUE :: values.toList
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value value of interest
    * @return list of indices where this value is stored
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // gets the indices related to a certain value
      val valueIndex = values.indices.
         par.find(index => values(index) == value).getOrElse(-1)

      // now gets the indices pointing to this value
      val result = if (valueIndex != -1) {
         indicesSets(valueIndex)
      }
      else {
         Set()
      }

      // return result converted to List
      result.toList
   }

   /**
    * Gets the list of indices defined for the store
    * (where a value different of default value is
    * stored)
    *
    * @return list of indices
    */
   override def getIndices: List[Long] = {
      indicesSets.par.flatMap(set => set.toList).toList
   }


   /**
    * Gets the proportion of zeros
    *
    * @return proportion of zeros
    */
   override def getZerosProportion: Double = {
      // gets the complete number of values represented by
      // the potential
      val size = variables.possibleValues

      // gets the number of indices related to values
      // different from 0
      val numberOfIndices =
                     indicesSets.map(set => set.size).sum

      // the complement is the number of 0's
      val numberZeros = size - numberOfIndices

      // return the desired proportion
      numberZeros / size
   }

   /**
    * gets the proportions of repetitions for each value
    *
    * @return lits of proportions for each one of the
    *         list of different values
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues
      val differentValues = getDifferentValues

      // gets the counter of indices for each value
      val counters = (1 until values.length).par.map(
         index => indicesSets(index).size
      )

      // computes and return the proportions for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      }).toList
   }

   /**
    * tuple with number of represented indices, number of
    * stored values and number of different values. In this
    * case the number of stored values is equals to the number
    * of different values
    *
    * @return
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of indices
      val numberIndices = indicesSets.map(set => set.size).sum

      // return the tuple with possible values, number of indices
      // stored in the potential, number of values stored and
      // number of different values (the same as the previous one)
      (numberIndices, values.length, values.length)
   }


   /**
    * toString method
    *
    * @return string with information about the object
    */
   override def toString: String = {
      var output = "ArraySIIStore (immutable store).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                           defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + indicesSets.mkString("\n") + "\n"
      output = output + "Different values: " + values.mkString(" ") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + " rep: " + "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 + "\n"
      output = output + "values stored: " + size._2 + "\n"
      output = output + "number of different values: " + size._3 + "\n"
      output = output + "memory size: " + getMemorySize + "\n"
      output
   }

   /**
    * Get object memory size
    *
    * @return estimation of memory size
    */
   def getMemorySize: Long = {
      // get size due to variables
      //val variableSizes = variables.getObjectSize
      val variableSizes = variables.getMemorySize

      // get size of indices
      val indicesSizes =
            indicesSets.map(set => set.size).sum * DataSizes.LONG

      // get size of values; adds one due to default value
      val valueSizes =
            values.length * DataSizes.DOUBLE + +DataSizes.DOUBLE

      // considers the size of both arrays and a number of sets equals
      // to the length of the arrays
      val structuresSize =
            DataSizes.ARRAY * 2 + DataSizes.SET * indicesSets.length

      // return the global sum
      variableSizes + indicesSizes + valueSizes + structuresSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      IDSIStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      IDSIStore.marginalizeDefault)
}

/**
 * Companion object offering factory methods
 */
object IDSIStore extends Combiner with Marginalizer {

   /**
    * Factory method
    *
    * @param variables domain of potential
    * @param values values to store
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): IDSIStore = {
      // sets the behavior for default value treatment
      //val defaultValueComputer =
      //      DefaultValueComputer(defaultValueComputerType)
      //val defaultValue =
      //      defaultValueComputer.computeDefaultValue(values)

      // consider the list of variables
      val result = variables.variableList match {
         // if no variables specified just creates an empty
         // store
         case Nil =>
            new IDSIStore(variables, Array(Set[Long]()), Array(0))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] =
               values.filter(value => value != Util.DEFAULTVALUE).
                 distinct

            // gets the indices for each value
            val indicesForVals: Array[Set[Long]] =
               differentValues.map(value => {
                  (0.toLong until values.length.toLong).
                     filter(index => values(index.toInt) == value).toSet
               })

            // now creates the object storing the values
            val newStore = new IDSIStore(variables, indicesForVals,
                           differentValues)

            // return newStore
            newStore
      }
      result
   }

   /**
    * creates a store for a single values and its corresponding
    * indexes
    * @param variables target variables for the store
    * @param value single value for the potential
    * @param indices set of incices with assigned value
    * @return
    */
   def apply(variables : VariableSet, value : Double,
             indices : Set[Long]) = {
      new IDSIStore(variables, Array(indices), Array(value))
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    * @note  TODO: method to be implemented. Different
    *        alternatives should be tested
    */
   override def combineDefault(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      null
   }

   /**
    * Marginalization method
    *
    * @param valst    potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    * @note  TODO: method to be implemented. Different
    *        alternatives should be tested
    */
   override def marginalizeDefault(valst: ValueStore, variable: Variable): ValueStore = {
      null
   }
}






