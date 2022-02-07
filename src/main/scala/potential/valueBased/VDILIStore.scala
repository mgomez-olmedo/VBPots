package potential.valueBased

import base.{Variable, VariableSet}
import potential.{ValueStoreTypes, _}
import utils.{DataSizes, Util}

import scala.collection.immutable.HashMap

/**
 * Class for storing values using the following structure:
 * dictionary with entries composed by:
 * - key: each one of the different values
 * - value: immutable list of indices where the value is
 * stored
 * @param variables domain of potential
 * @param map dictionary with all the information
 */
case class VDILIStore(variables: VariableSet,
                      map: Map[Double, List[Long]])
                       extends ValueDrivenStore with Combiner with Marginalizer {
   /**
    * default value strategy
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDILISTIMMUT

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value stored in position given by index
    */
   def getValue(index: Long): Double = {
      VDILIStore.addGetValueCalls

      // find the index in the map
      val result = map.find(entry => entry._2.contains(index)) match {
         case Some((x, _)) => x
         case None => Util.DEFAULTVALUE
      }

      // return result
      result
   }

   /**
    * adds a new value to the map
    *
    * @param value value to add
    * @param index index where the value is stored
    */
   def addValueForRepresentation(value: Double, index: Long): VDILIStore = {
      val indicesForValue: List[Long] =
            map.getOrElse(value, null)
      val res = if (indicesForValue != null) {
         map + (value -> (index :: indicesForValue))
      }
      else {
         map + (value -> List(index))
      }

      // creates a copy of the object (it can be used just
      // because only one data member changes)
      val result =
         VDILIStore(variables, res.keys.toArray)

      // return result
      result
   }

   /**
    * produces a collection of stores: one for each
    * value
    *  @return
    */
   override def split: List[ValueStore] = {
      map.keySet.map(value => {
         VDILIStore(variables, value, map.get(value).get)
      }).toList
   }

   /**
    * Gets the complete list of values
    *
    * @return list of values
    */
   override def getListValues: List[Double] = {
      // return the list of values
      (0L until variables.possibleValues).
         map(index => getValue(index)).toList
   }

   /**
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues: List[Double] = {
      Util.DEFAULTVALUE :: map.keySet.toList
   }

   /**
    * Gets the complete list of values, including repetitions
    *
    * @return list of values
    */
   def getCompleteListValues: List[Double] = {

      // considers each index and gets its value
      (0L until variables.possibleValues).map(index => {
         getValue(index)
      }).toList
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value value of interest
    * @return list of indices storing the target value
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // just retrieve the entry for this value
      map.get(value).orNull
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices stored
    */
   override def getIndices: List[Long] = {
      getDifferentValues.flatMap(value => getIndicesForValue(value))
   }

   /**
    * Gets the proportion of zeros
    *
    * @return proportion of zeros
    */
   override def getZerosProportion: Double = {
      // gets the complete number of values represented
      // by the potential
      val size = variables.possibleValues

      // gets the number of indices
      val numberOfIndices = map.keySet.map(value => {
         map(value).length
      }).sum

      val numberZeros = size - numberOfIndices

      // return the desired proportion
      numberZeros / size
   }

   /**
    * gets the proportions of repetitions for each value
    *
    * @return proportions of repetitions
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters: List[Int] = map.keySet.map(value => {
         map(value).length
      }).toList

      // computes and return the proportions for each
      // value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets the size of the store as a tuple containing
    * indices stored, values stored and number of
    * different values
    *
    * @return tuple with info
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val values = map.keySet.size

      // determine the number of indices
      val numberIndices =
            map.map(entry => entry._2.length).sum

      // return the tuple with possible values, number
      // of indices stored in the potential, number of
      // values stored and number of different values +
      // (the same as the previous one)
      (numberIndices, values, values)
   }

   /**
    * toString method
    *
    * @return string with info about the object
    */
   override def toString: String = {
      var output = "MapLIIStore (immutable).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //               defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nValues: "
      output = output + map.mkString("\n") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 + "\n"
      output = output + "values stored: " + size._2 + "\n"
      output = output + "number of different values: " + size._3 + "\n"
      output = output + "memory size: " + getMemorySize
      output
   }

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {
      // gets the size due to the number of variables
      //val variablesSizes = variables.getObjectSize
      val variablesSizes = variables.getMemorySize

      // gets sizes due to the map; adds one due to
      // default value
      val mapSizes =
         map.size * DataSizes.DOUBLE + DataSizes.DOUBLE

      // gets sizes due to indices
      val indicesSizes =
         map.values.map(list => list.size * DataSizes.LONG).sum

      // size due to data structures
      val structuresSize =
         DataSizes.MAP + map.size * DataSizes.ARRAY

      // return the sum of sizes
      variablesSizes + mapSizes + indicesSizes + structuresSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      VDILIStore.super.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      VDILIStore.super.marginalizeDefault)

   /**
    * Abstract method for pruning
    *
    * @param threshold maximum loss of entropy
    * @return
    */
   override def prune(threshold: Double): ValueDrivenStore = ???

   /**
    * merge two entries of the store producing a new one
    *
    * @param value1
    * @param value2
    * @return
    */
   override def merge(value1: Double, value2: Double): ValueDrivenStore = ???
}

/**
 * Companion object acting as factory
 */
object VDILIStore extends Combiner with Marginalizer {
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values values to store
    * @return
    */
   def apply(variables: VariableSet, values: Array[Double]): VDILIStore = {
      // sets default value using the associated trait: in this case
      // the default value is obtained with the analysis of the sequence
      // of values
      //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //   defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an empty store
         case Nil =>
            val valueList = List()
            new VDILIStore(variables, HashMap(0.0 -> valueList))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] = values.
               filter(value => value != Util.DEFAULTVALUE).distinct

            // gets the indices for each value
            val indicesForVal: Array[Array[Long]] = differentValues.map(value => {
               (0.toLong until values.length.toLong).filter(index => values(index.toInt) == value).toArray
            })

            // For each array of indices (related to the same value) get the corresponding
            // list of indices
            val result: Map[Double, List[Long]] = differentValues.indices.map(index => {
               differentValues(index) -> indicesForVal(index).toList
            }).toMap

            // now creates the object storing the values. The final call to seq
            // is required for getting a sequential collection at the end
            new VDILIStore(variables, result.seq)
      }
   }

   /**
    * creates a new store for a values and its indexes
    * @param variables target variables
    * @param value value to store
    * @param indices indexed with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, indices : List[Long]) = {
      new VDILIStore(variables, HashMap(value -> indices))
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    * @note TODO: to be implemented
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
    * @note TODO: to be implemented
    */
   override def marginalizeDefault(valst: ValueStore, variable: Variable): ValueStore = {
      null
   }
}




