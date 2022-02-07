package potential.valueBased

import base.{Variable, VariableSet}
import potential.grain.{Grain, GrainSet}
import potential.{ValueStoreTypes, _}
import utils.{DataSizes, Util}

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, TreeSet}

/**
 * Class for storing values using the following structure:
 * dictionary with values as keys and set of grains as values
 * @param variables potential domain
 * @param map dictionary with info about indices and values
 */
case class VDGSStore(variables: VariableSet,
                     map: Map[Double, GrainSet]) extends ValueDrivenStore
                      with Combiner with Marginalizer{
   /**
    * Default value strategy
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDGSSTORE

   /**
    * Return the list of variables
    *
    * @return set of variables in potential
    */
   override def getVariables: VariableSet = variables

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value
    */
   override def getValue(index: Long): Double = {
      VDGSStore.addGetValueCalls

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
    * @param index index where the value is stored
    * @note TODO: review this method ---- how to
    *       efficiently aggregate a new index to a
    *       set of grains?
    */
   def addValueForRepresentation(value: Double, index: Long): VDGSStore = {
      // gets the set of grains for the value. The
      // result is a certain set or null (there is
      // no set for the value passed as argument)
      val setForValue: GrainSet =
                     map.getOrElse(value, null)

      // if there is a set for the value, then adds
      // an index and replace the entry for the value.
      // If there are no set for the value, just create
      // a new set for the index
      val res: Map[Double, GrainSet] = if (setForValue != null) {
         map + (value -> setForValue.addIndex(index))
      }
      else {
         map + (value -> GrainSet(TreeSet[Grain](Grain(index))))
      }

      // return res
      VDGSStore(variables, res.keys.toArray)
   }

   /**
    * split the store into a new list of stores, one per value
    *  @return
    */
   override def split: List[ValueStore] = {
      map.keySet.map(value => VDGSStore(variables, value, map(value))).toList
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
   override def getListValues: List[Double] = {
      // compose a complete list of values
      val values = List()

      // considers each index
      (0L until variables.possibleValues).
         foreach(index => {
            getValue(index) :: values
      })

      // return values after reverting
      values.reverse
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value target value
    * @return list of indices where this value is
    *         stored
    */
   override def getIndicesForValue(value: Double): List[Long] = {
      // gets the grains for this value
      val grains = map.get(value).orNull

      if (grains != null) {
         // consider each grain and applies a function on
         // it for adding the indices contained in it
         grains.getIndices
      }
      else {
         List()
      }
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices stored
    */
   override def getIndices: List[Long] = {
      getDifferentValues.flatMap(
         value => getIndicesForValue(value))
   }

   /**
    * Gets the proportion of zeros
    *
    * @return proportion of zeros
    */
   override def getZerosProportion: Double = {
      // gets the complete number of values
      // represented by the potential
      val size = variables.possibleValues

      // gets the number of indices represented
      // with grains
      val numberOfIndices = map.keySet.map(value => {
         map(value).getNumberOfIndices
      }).sum

      val numberZeros = size - numberOfIndices

      // return the desired proportion
      numberZeros / size
   }

   /**
    * gets the proportions of repetitions for each value
    *
    * @return
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters: List[Int] = map.keySet.map(value => {
         val grains = map(value)
         grains.getNumberOfIndices.toInt
      }).toList

      // computes and return the proportions for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets the size of the store: a tuple with number
    * of indices stored, number of values and number
    * of different values
    *
    * @return tuple with the information described above
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val numberValues = map.keySet.size

      // gets the number of indices stored in the
      // complete potential
      val numberIndices = map.map(entry => {
         entry._2.getNumberOfIndices
      }).sum

      // return the tuple with possible values, number
      // of indices stored in the potential, number of
      // values stored and number of different values
      // (the same as the previous one)
      (numberIndices, numberValues, numberValues)
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

      // gets sizes due to the map and the keys. Adds 1
      // due to default value
      val mapSizes = map.size * DataSizes.DOUBLE + DataSizes.DOUBLE

      // gets sizes due to grains
      val grainSizes =
         map.values.map(grainSet => grainSet.getMemorySize).sum

      // size due to data structures
      val structuresSize =
         DataSizes.MAP + map.size * DataSizes.SET

      // return the sum of sizes
      variablesSizes + mapSizes + grainSizes + structuresSize
   }

   /**
    * toString method
    *
    * @return string with object info
    */
   override def toString: String = {
      var output = "MapSGStore (mutable store).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                           defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + map.keySet.map(value => {
         value.toString + "-> " + map.get(value).toString + "\n"
      }).mkString(" ") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 + "\n"
      output = output + "values stored: " + size._2 + "\n"
      output = output + "number of different values: " + size._3 + "\n"
      output = output + "memory size: " + getMemorySize + "\n"
      output
   }

   /**
    * Gets the complete number of grains
    *
    * @return number of grains
    */
   def getNumberGrains: Int = {
      map.values.map(grainSet => grainSet.getNumberGrains).sum
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      VDGSStore.combineDefault)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      VDGSStore.marginalizeDefault)

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
object VDGSStore extends Combiner with Marginalizer{
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values potential values
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): VDGSStore = {
      // sets default value using the associated trait: in
      // this case the default value is obtained with the
      // analysis of the sequence of values
      //val defaultValueComputer =
      //   DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //   defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an empty store
         case Nil =>
            val grain = new Grain(0, 0)
            val valueSet = TreeSet(grain)
            new VDGSStore(variables, HashMap(0.0 -> GrainSet(valueSet)))

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
            // value) get the corresponding set of grains
            val result: Seq[(Double, GrainSet)] =
               differentValues.indices.map(index => {
                differentValues(index) ->
                     GrainSet(fromIndicesToGrains(indicesForVal(index)))
            })

            // now creates the object storing the values. The
            // final call to seq is required for getting a
            // sequential collection at the end
            new VDGSStore(variables, result.toMap.seq)
      }
   }

   /**
    * creates a new object for a single value and with the same
    * list of grains passed as argument
    * @param variables target variables
    * @param value value to store
    * @param grainSet indexed with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, grainSet : GrainSet): VDGSStore = {
      new VDGSStore(variables, Map(value -> grainSet.copy()))
   }


   /**
    * Auxiliary method for converting an array of indices
    * into a corresponding set of grains
    *
    * @param indices array of indices
    * @return tree of grains
    */
   def fromIndicesToGrains(indices: Array[Long]): TreeSet[Grain] = {
      /*
       * Auxiliary method for converting sequences of indices
       * into grains
       * @param indices indices to convert
       * @param start initial position of the index to consider
       * @param tree tree of grains
       * @return
       */
      @tailrec
      def convertSequences(indices: Array[Long], start: Int,
                           tree: TreeSet[Grain]): TreeSet[Grain] = {
         // gets the sequence of consecutive indices (or -1
         // if that sequence is not found)
         val end = (start until indices.length - 1).
            indexWhere(index => indices(index) != indices(index + 1) - 1)
         val finalEnd = if (end == -1) indices.length - 1 else start + end
         val sequence = (start, finalEnd)

         // makes a new grain for the sequence and add it
         // to the tree passed as argument
         val newTree = tree + Grain(indices(sequence._1),
                                    indices(sequence._2))

         // produce a new call to convertSequences if needed
         if (sequence._2 + 1 < indices.length)
            convertSequences(indices, sequence._2 + 1, newTree)
         else
            newTree
      }

      // produce the initial call to convertSequences
      convertSequences(indices, 0, TreeSet[Grain]())
   }

   /**
    * Combination strategy
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
    * Marginalization strategy
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

