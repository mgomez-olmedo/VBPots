package potential.valueBased

import base.{Variable, VariableSet}
import potential.grain.{Grain, GrainList}
import potential.{ValueStoreTypes, _}
import utils.{DataSizes, Util}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

/**
 * Class for storing values using the following structure:
 * dictionary of entries where each entry contains:
 * - key: one of the different values of the potential
 * - value: list of grains containing indices where the
 * value acting as key is stored
 *
 * @param variables potential domain
 * @param map       dictionary with information
 */
case class VDGLStore(variables: VariableSet,
                     map: Map[Double, GrainList]) extends ValueDrivenStore
   with Combiner with Marginalizer {
   /**
    * Default value strategy
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDGLSTORE

   /**
    * Return the list of variables
    *
    * @return
    */
   override def getVariables: VariableSet = variables

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value stored in the position given by index
    */
   override def getValue(index: Long): Double = {
      VDGLStore.addGetValueCalls

      // find the index in the map
      val entry = map.find(entry => {
         entry._2.findIndex(index) != null
      }).orNull

      // extracts the double value or return 0.0
      entry match {
         case null => Util.DEFAULTVALUE
         case (v1, _) => v1
      }
   }

   /**
    * adds a new value to the map
    *
    * @param value value to add
    * @param index index for the value
    *              TODO: review this method ----
    *              how to efficiently aggregate a new index to a set of grains?
    * @return new storage after the operation
    */
   def addValueForRepresentation(value: Double, index: Long): VDGLStore = {
      // gets the set of grains for the value. The result
      // is a certain set or null (there is no set for the
      // value passed as argument)
      val setForValue: GrainList = map.getOrElse(value, null)

      // if there is a set for the value, then adds an
      // index and replace the entry for the value. If
      // there are no set for the value, just create a
      // new set for the index
      val res: Map[Double, GrainList] = if (setForValue != null) {
         map + (value -> setForValue.addIndex(index))
      }
      else {
         map + (value -> GrainList(List[Grain](Grain(index))))
      }

      // return res
      VDGLStore(variables, res.keys.toArray)
   }

   /**
    * split the store into a new list of stores, one per value
    *
    * @return
    */
   override def split: List[ValueStore] = {
      map.keySet.map(value => VDGLStore(variables, value, map(value))).toList
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
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues: List[Double] = {
      map.keySet.toList.sorted
   }

   /**
    * Gets the complete list of values, including
    * repetitions
    *
    * @return complete list of values
    */
   def getCompleteListValues: List[Double] = {
      // compose a complete list of values
      val values = List()

      // considers each index
      (0L until variables.possibleValues).foreach(index => {
         getValue(index) :: values
      })

      // return values after reverting
      values.reverse
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value value of interest
    * @return list of indices where the value is stored
    */
   override def getIndicesForValue(value: Double): List[Long] = {
      // gets the grains for this value
      val grains = map.get(value).orNull

      if (grains != null) {
         // consider each grain and applies a function
         // on it for adding the indices contained in it
         grains.getIndices
      }
      else {
         List()
      }
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices
    */
   override def getIndices: List[Long] = {
      getDifferentValues.
         flatMap(value => getIndicesForValue(value))
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

      // gets the number of indices represented with
      // grains
      val numberOfIndices = map.keySet.map(value => {
         map(value).getSize
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
         map(value).getSize.toInt
      }).toList

      // computes and return the proportions for each
      // value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets size of the store: indices, values and different
    * values
    *
    * @return
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val numberValues = map.keySet.size

      // compute the number of indices
      val numberIndices = map.map(entry => entry._2.getSize).sum

      // return the tuple with possible values, number of
      // indices stored in the potential, number of values
      // stored and number of different values (the same
      // as the previous one)
      (numberIndices, numberValues, numberValues)
   }

   /**
    * Gets memory size for an object of this class
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {

      // gets the size due to the number of variables
      //val variablesSizes = variables.getObjectSize
      val variablesSizes = variables.getMemorySize

      // gets sizes due to the map and the keys; adds one
      // due to default value
      val mapSizes = map.size * DataSizes.DOUBLE + DataSizes.DOUBLE

      // gets sizes due to grains
      val grainSizes =
         map.values.map(grainList => grainList.getMemorySize).sum

      // size due to data structures
      val structuresSize =
         DataSizes.MAP + map.size * DataSizes.ARRAY

      // return the sum of sizes
      variablesSizes + mapSizes + grainSizes + structuresSize
   }

   /**
    * Gets the complete number of grains
    *
    * @return grains counter
    */
   def getNumberGrains: Int = {
      map.values.map(grainList => grainList.getNumberGrains).sum
   }

   /**
    * return information about grains: number of grains, max
    * length of grain, average length of grains
    *
    * @return tuple with number of grains - max length - average length
    */
   def getGrainsInfo: (Int, Long, Double) = {
      val number = getNumberGrains
      val maxLength = map.values.map(grainList => grainList.getMaxLength).max
      val avgLength = map.values.map(grainList => grainList.getAverageLength).sum / (map.values.size * 1.0)

      // return the tuple
      (number, maxLength, avgLength)
   }

   /**
    * return information about length of grains
    *
    * @return map with entries for length - number of grains
    */
   def getGrainsLength = {

      // get grains info for detecting the max length of all the grains
      val maxLength = getGrainsInfo._2

      // considers all the lengths and count how many grains has this length
      (1L to maxLength).map(length => {
         val lengthCounter: Int = map.values.map(granList =>
            granList.filter(grain => grain.getSize == length).size).filter(counter => counter != 0).sum
         (length, lengthCounter)
      }).toMap.filter(entry => entry._2 != 0)
   }

   /**
    * toString method
    *
    * @return string with information about the object
    */
   override def toString: String = {
      var output = "MapLGStore .......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                           defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + getDifferentValues.map(value => {
         value.toString + "-> " + map.get(value).toString + " "
      }).mkString("\n") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 + "\n"
      output = output + "values stored: " + size._2 + "\n"
      output = output + "number of different values: " + size._3 + "\n"
      output = output + "memory size: " + getMemorySize + "\n"
      output = output + "grains info \n"
      val grainsInfo = getGrainsInfo
      output = output + "number of grains: " + grainsInfo._1 + "\n"
      output = output + "max length of grains: " + grainsInfo._2 + "\n"
      output = output + "average size of grains: " + grainsInfo._3 + "\n"
      // shows info about the counters with the length of the grains
      output += "counter of length of grains \n"
      val counters = getGrainsLength
      output += counters.toString
      output
   }

   /**
    * merge two entries and produces a new map with a single entry
    * for the resultant value
    *
    * @param value1
    * @param value2
    * @return
    */
   override def merge(value1: Double, value2: Double): VDGLStore = {
      val data1 = computeSumAndSizeForValue(value1)
      val data2 = computeSumAndSizeForValue(value2)
      val newValue = (data1._1 + data2._1) / (data1._2 + data2._2)

      // gets both list of grains
      val grains1 = map.get(value1).get
      val grains2 = map.get(value2).get

      // merge both of them
      val grains: GrainList = GrainList.merge(grains1, grains2)

      // creates a new map removing entries for merged values
      val reduced: Map[Double, GrainList] = map - value1 - value2

      // creates a new VDGLStore with the result
      new VDGLStore(variables, reduced + (newValue -> grains))
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      ValueStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      ValueStore.marginalizeDefault)
}

/**
 * Companion object offering factory methods
 */
object VDGLStore extends Combiner with Marginalizer {
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls += 1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values    values of potential
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): VDGLStore = {
      // sets default value using the associated trait:
      // in this case the default value is obtained with
      // the analysis of the sequence of values
      //val defaultValueComputer =
      //      DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //      defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an empty store
         case Nil =>
            val grain = new Grain(0, 0)
            val valueSet = GrainList(List(grain))
            new VDGLStore(variables, HashMap(0.0 -> valueSet))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] =
               values.filter(value => value != Util.DEFAULTVALUE).distinct

            // gets the indices for each value
            val indicesForVal: Array[Array[Long]] =
               differentValues.map(value => {
                  (0.toLong until values.length.toLong).
                     filter(index => values(index.toInt) == value).toArray
               })

            // For each array of indices (related to the
            // same value) get the corresponding set of grains
            val result: Map[Double, GrainList] =
            differentValues.indices.map(index => {
               differentValues(index) ->
                  GrainList(fromIndicesToGrains(indicesForVal(index)))
            }).toMap

            // now creates the object storing the values
            new VDGLStore(variables, result)
      }
   }

   /**
    * creates a new object for a single value and with the same
    * list of grains passed as argument
    *
    * @param variables variables for potential domain
    * @param value     value to store
    * @param grainList list of grains for the value
    * @return
    */
   def apply(variables: VariableSet, value: Double, grainList: GrainList): VDGLStore = {
      new VDGLStore(variables, HashMap(value -> grainList.copy()))
   }

   /**
    * Auxiliary method for converting an array of indices
    * into a corresponding set of grains
    *
    * @param indices array of indices
    * @return list of grain objects
    */
   def fromIndicesToGrains(indices: Array[Long]): List[Grain] = {
      /*
       * Auxiliary method for converting sequences of indices
       * into grains
       * @param indices indices to convert
       * @param start initial position of the index to consider
       * @param list tree of grains
       * @return list of grains
       */
      @tailrec
      def convertSequences(indices: Array[Long], start: Int, list: List[Grain]): List[Grain] = {
         // gets the sequence of consecutive indices (or -1
         // if that sequence is not found)
         val end = (start until indices.length - 1).
            indexWhere(index => indices(index) != indices(index + 1) - 1)
         val finalEnd = if (end == -1) indices.length - 1
         else start + end
         val sequence = (start, finalEnd)

         // makes a new grain for the sequence and add it
         // to the list passed as argument
         val newList = Grain(indices(sequence._1),
            indices(sequence._2)) :: list

         // produce a new call to convertSequences if needed
         if (sequence._2 + 1 < indices.length)
            convertSequences(indices, sequence._2 + 1, newList)
         else
            newList
      }

      // produce the initial call to convertSequences
      convertSequences(indices, 0, List[Grain]())
   }
}





