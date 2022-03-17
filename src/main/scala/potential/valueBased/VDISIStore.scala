package potential.valueBased

import base.{Variable, VariableSet}
import potential._
import utils.{DataSizes, Util}

import scala.collection.immutable.HashMap

/**
 * Class for storing values using the following structure:
 * dictionary with pairs (value - set (immutable) of indices
 * where the value is stored
 * @param variables potential domain
 * @param map dictionary with the info
 */
case class VDISIStore(variables: VariableSet,
                      map: Map[Double, Set[Long]]) extends ValueDrivenStore
                       with Combiner with Marginalizer{
   /**
    * default value strategy
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDISISTORE

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value related to the index
    */
   def getValue(index: Long): Double = {
      VDISIStore.addGetValueCalls

      // find the index in the map
      map.find(entry => entry._2.contains(index)).getOrElse((Util.DEFAULTVALUE, Set()))._1
   }

   /**
    * adds a new value to the map
    *
    * @param value new value to add
    * @param index index where the value is stored
    * @return new object after the operation
    */
   def addValueForRepresentation(value: Double, index: Long): VDISIStore = {
      val indicesForValue: Set[Long] = map.getOrElse(value, null)

      // get the result of adding a new entry
      val res = if (indicesForValue != null) {
         map + (value -> (indicesForValue + index))
      }
      else {
         map + (value -> Set(index))
      }

      // creates a copy of the object (it can be used just
      // because only one data member changes)
      val result = VDISIStore(variables, res)

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
         VDISIStore(variables, value, map(value))
      }).toList
   }

   /**
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues: List[Double] = {
      map.keySet.toList.sorted
   }

   /**
    * Gets the complete list of values, including repetitions
    *
    * @return complete list of values
    */
   override def getListValues: List[Double] = {

      // considers each index and gets its value
      (0L until variables.possibleValues).map(index => {
         getValue(index)
      }).toList
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value value of interest
    * @return list of indices where the value is stored
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // just retrieve the entry for this value
      map(value).toList
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices where the value is stored
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

      // gets the number of indices represented with grains
      val numberOfIndices = map.keySet.map(value => {
         map(value).size
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
      val counters: List[Int] =
         map.keySet.
            map(value => map(value).size).toList

      // computes and return the proportions for
      // each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets the size of the store
    *
    * @return tuple with number of indices stored, number
    *         of values stored and number of different
    *         values
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val values = map.keySet.size

      // determine the number of indices
      val numberIndices =
                  map.map(entry => entry._2.size).sum

      // return the tuple with possible values, number of
      // indices stored in the potential, number of values
      // stored and number of different values (the same
      // as the previous one)
      (numberIndices, values, values)
   }

   /**
    * toString method
    *
    * @return string with object information
    */
   override def toString: String = {
      var output = "MapSIIStore (immutable).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                        defaultValueComputer.getType + "\n"
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
         map.values.map(set => set.size * DataSizes.LONG).sum

      // size due to data structures
      val structuresSize =
         DataSizes.MAP + map.size * DataSizes.SET

      // return the sum of sizes
      variablesSizes + mapSizes + indicesSizes + structuresSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      ValueStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      ValueStore.marginalizeDefault)

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

      // gets both list of grains
      val indices1 = map.get(value1).get
      val indices2 = map.get(value2).get

      // merge both sets of indices
      val indices = indices1 ++ indices2

      // creates a new map removing entries for merged values
      val reduced: Map[Double, Set[Long]] = map - value1 - value2

      // creates a new VDILIStore with the result
      new VDISIStore(variables, reduced + (newValue -> indices))
   }
}

/**
 * Companion object acting as factory
 */
object VDISIStore extends Combiner with Marginalizer{
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values values stored in the potential
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): VDISIStore = {
      // sets default value using the associated trait:
      // in this case the default value is obtained with the
      // analysis of the sequence of values
      //val defaultValueComputer =
      //   DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //   defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an empty store
         case Nil =>
            val valuesSet: Set[Long] = Set()
            new VDISIStore(variables, HashMap(0.0 -> valuesSet))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] = values.
               filter(value => value != Util.DEFAULTVALUE).distinct

            // gets the indices for each value
            val indicesForVal: Array[Array[Long]] =
               differentValues.map(value => {
                  (0.toLong until values.length.toLong).
                     filter(index => values(index.toInt) == value).toArray
            })

            // For each array of indices (related to the same
            // value) get the corresponding list of indices
            val result: Map[Double, Set[Long]] =
               differentValues.indices.map(index => {
                  differentValues(index) -> indicesForVal(index).toSet
            }).toMap

            // now creates the object storing the values.
            new VDISIStore(variables, result.seq)
      }
   }

   /**
    * creates a new store for a values and its indexes
    * @param variables target variables
    * @param value value to store
    * @param indices indexed with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, indices : Set[Long]): VDISIStore = {
      new VDISIStore(variables, Map(value -> indices))
   }
}











