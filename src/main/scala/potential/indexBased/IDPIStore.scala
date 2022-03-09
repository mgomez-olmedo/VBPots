package potential.indexBased

import base.{Variable, VariableSet}
import potential._
import utils.{DataSizes, Util}

import scala.+:
import scala.collection.mutable.ArrayBuffer

/**
 * Class for storing values as a value driven store with
 * the following structure: different values are stored
 * in a immutable array; another array contains pairs with
 * information about index of potential - index in array
 * of values
 * @constructor creates a new object using the information
 *              passed as argument
 * @param variables store domain
 * @param indices indices containing non zero values
 * @param values values different to zero
 */
case class IDPIStore(variables: VariableSet,
                     indices: Array[(Long, Long)],
                     values: Array[Double])  extends ValueDrivenStore {

   // sets default value using the associated trait: in
   // this case the default value is obtained with the
   // analysis of the sequence of values

   // Set the kind of store
   override val kind: ValueStoreTypes.Value =
                     ValueStoreTypes.IDPISTORE

   /**
    * Gets the value for a corresponding index
    *
    * @param index target index
    * @return value store in the index passed as argument
    */
   def getValue(index: Long): Double = {
      IDPIStore.addGetValueCalls

      // checks the array of indices looking for a pair
      // containing the index as first element
      val result: (Long, Long) =  indices.find(pair => pair._1 == index).
                  getOrElse((index, -1))

      // compose final result in order to return defaultValue
      // if the index was not found
      if (result._2 == -1) Util.DEFAULTVALUE else
         values(result._2.toInt)
   }

   def getValue2(index : Long) : Double = {
      def go(tuples: List[(Long, Long)]) : Double = {
         tuples match{
            case head::tail => {
               //println("looking for: " + index+ " -> considering index: " + head._1)
               if(head._1 == index) values(head._2.toInt)
               else go(tail)
            }
            case _ =>  Util.DEFAULTVALUE
         }
      }

      go(indices.toList)
   }

   def getValue3(index : Long) : Double = {
      val indexInValues = indices.indexWhere(_._1 == index)
      if(indexInValues == -1) Util.DEFAULTVALUE
      else values(indexInValues)
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
    * adds a value to the store
    *
    * @param value value to add
    * @param index index of new value
    */
   def addValueForRepresentation(value: Double, index: Long): IDPIStore = {
      // checks if the value is already contained into
      // values
      val indexForValue =
         values.indices.par.find(index => values(index) == value).getOrElse(-1)

      // if the value is already defined, add a new
      // entry to indices. In any other case it is needed
      // to add a new value
      val newIndices: Array[(Long, Long)] =
         if (indexForValue != -1)
            indices ++ Array((index, indexForValue.toLong))
         else
            indices ++ Array((index, values.length.toLong))

      // for new values, add it
      val newValues = if (indexForValue == -1)
                           values ++ Array(value) else values

      // creates a copy of the object
      new IDPIStore(variables, newIndices, newValues)
   }

   /**
    * split the store into a new list of stores, one per value
    *  @return
    */
   override def split: List[ValueStore] = {
      (0L until values.length).map(index => {
         val indexesValue = indices.filter(_._2 == index).map(_._2)
         IDPIStore(variables, values(index.toInt), indexesValue)
      }).toList
   }

   /**
    * Gets the complete list of values
    *
    * @return list of values
    */
   override def getListValues: List[Double] = {
      (0L until variables.possibleValues).
                     map(index => getValue(index)).toList
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value target value
    * @return list of indices
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // gets the indices related to a certain value
      val valueIndex =
         values.indices.par.find(index => values(index) == value).getOrElse(-1)

      // now gets the indices pointing to this value
      val result = if (valueIndex != -1) {
         indices.par.filter(pairs => pairs._2 == valueIndex).
            map(pair => pair._1).toList
      }
      else {
         List()
      }

      // return result
      result
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices
    */
   override def getIndices: List[Long] = {
      indices.par.map(entry => entry._1).toList
   }

   /**
    * Gets the proportion of zeros
    *
    * @return proportion of xero values
    */
   override def getZerosProportion: Double = {
      // gets the complete number of values represented
      // by the potential
      val size = variables.possibleValues

      // gets the number of indices related to values
      // different from 0
      val numberOfIndices = indices.length

      val numberZeros = size - numberOfIndices

      // return the desired proportion
      numberZeros / size
   }

   /**
    * gets the proportions of repetitions for each value
    *
    * @return list of values
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters = values.par.map(value => {
         indices.map(pair => pair._2).
            map(indexInValues =>
               values(indexInValues.toInt) == value).length
      })

      // computes and return the proportions for each
      // value
      counters.map(x => {
         x / concreteValues.size.toDouble}).toList
   }

   /**
    * Gets the size of the store
    *
    * @return tuple with number of indices stored, number
    *         of values stored and number of different
    *         values
    */
   override def getSize: (Long, Long, Long) = {
      // return a tuple with possible values, number of
      // indices with assigned values (length of indices
      // data member), number of values stored (length of
      // values data member) and number of different values
      // (equals to the number of stored values)
      (indices.length, values.length, values.length)
   }

   /**
    * toString method
    *
    * @return string with object information
    */
   override def toString: String = {
      var output: String =
         "ArrayIIStore (immutable store).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                        defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + indices.mkString("\n") + "\n"
      output = output + "Different values: " +
                        values.mkString(" ") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE +
                        "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 +
                        "\n"
      output = output + "values stored: " + size._2 +
                        "\n"
      output = output + "number of different values: " +
                        size._3 + "\n"
      output = output + "memory size: " + getMemorySize
      output
   }

   /**
    * Get object size
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {
      // get size due to variables
      // val variableSizes = variables.getObjectSize
      val variableSizes = variables.getMemorySize

      // get size of indices: each entry of the indices
      // part contains an index (configuration index)
      // and the index of the value related to the
      // configuration
      val indicesSizes = indices.length * DataSizes.LONG * 2

      // get size of values. Adds 1 due to default value
      val valueSizes = values.length * DataSizes.DOUBLE +
                  DataSizes.DOUBLE

      // size due to data structures
      val structuresSize = 2 * DataSizes.ARRAY

      // return the global sum adding the size of the
      // arrays used for storing values and pairs
      variableSizes + indicesSizes + valueSizes +
                  structuresSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      IDPIStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      IDPIStore.marginalizeDefault)

   /**
    * merge two entries of the store producing a new one
    *
    * @param value1
    * @param value2
    * @return
    */
   override def merge(value1: Double, value2: Double): ValueDrivenStore = {
      val data1 = computeSumAndSizeForValue(value1)
      val data2 = computeSumAndSizeForValue(value2)
      val newValue = (data1._1 + data2._1) / (data1._2 + data2._2)

      // gets the positions of data1 and data2 in the array
      // of values
      val value1Index = values.indexWhere(_ == value1, 0)
      val value2Index = values.indexWhere(_ == value2, 0)

      // add a new value to the array of values
      var newValues = values :+ newValue

      // remove value1 and value2
      newValues = newValues.filter(value => value != value1 && value != value2)

      // gets the index of the new value
      val newValueIndex = newValues.indexWhere(_ == newValue, 0).toLong

      // update entries in the array changing values related to
      // value1Index and value2Index by newValueIndex
      val newIndices: Array[(Long, Long)] = indices.map(entry => {
         if(entry._2 == value1Index) (entry._1 -> newValueIndex)
         else
            if(entry._2 == value2Index) (entry._1 -> newValueIndex)
            else (entry._1 -> entry._2)
      })

      // creates a new object
      new IDPIStore(variables, newIndices, newValues)
   }
}

/**
 * Companion object
 */
object IDPIStore extends Combiner with Marginalizer {
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables domain of the store
    * @param values values to store
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): IDPIStore = {
      // sets the behavior for default value treatment
      //val defaultValueComputer =
      //         DefaultValueComputer(defaultValueComputerType)
      //val defaultValue =
      //         defaultValueComputer.computeDefaultValue(values)

      // consider the list of variables
      val result = variables.variableList match {
         // if no variables specified just creates an
         // empty store
         case Nil =>
            new IDPIStore(variables, Array(), Array(0))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] =
               values. filter(value => value != Util.DEFAULTVALUE).distinct

            // stores the indices for each value
            val arrayIndices =
               ArrayBuffer[(Long, Long)]()

            differentValues.indices.foreach(indexOfValue => {
               val indicesForValue =
                  (0.toLong until values.length.toLong).
                     filter(index => values(index.toInt) ==
                              differentValues(indexOfValue)).toArray
               indicesForValue.foreach(index => {
                  arrayIndices += ((index, indexOfValue))
               })
            })

            // now creates the object storing the values. The
            // final call to seq is required for getting a
            // sequential collection at the end
            val newStore = new IDPIStore(variables, arrayIndices.toArray,
               differentValues)

            // return newStore
            newStore
      }
      result
   }

   /**
    * Creates a store for a single value
    * @param variables target variables for the potential
    * @param value value to assign
    * @param indices indices where the value is assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, indices : Array[Long]): IDPIStore = {
      new IDPIStore(variables, indices.map(index => (index, 0L)).toArray,
         Array(value))
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    *         TODO: method to be implemented. We
    *         must consider different alternatives
    */
   override def combineDefault(valst1: ValueStore, valst2: ValueStore):
                                                ValueStore = {
      null
   }

   /**
    * Marginalization method
    *
    * @param valst    potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    *         TODO: method to be implemented. We
    *         *         must consider different alternatives
    */
   override def marginalizeDefault(valst: ValueStore, variable: Variable):
                                             ValueStore = {
      null
   }
}





