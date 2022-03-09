package potential.valueBased

import base.{Variable, VariableSet}
import mapper.{CombineMapper, MarginalizeMapper}
import potential._
import potential.indexBased.IDSMStore
import utils.{DataSizes, Util}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ParMap

/**
 * Class for storing values using the structure:
 *
 * @param variables variables in store domain
 * @param map dictionary with pairs entry -value as:
 * - key: each one of the different values to store
 * - value: mutable list of indices related to key
 * value
 */
case class VDILMStore(variables: VariableSet,
                      map: mutable.Map[Double, List[Long]])
   extends ValueDrivenStore with Combiner with Marginalizer {

   /**
    * default value strategy
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of storage
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDILMSTORE

   /**
    * Gets the value for a corresponding index
    *
    * @param index index of interest
    * @return value stored at index
    */
   def getValue(index: Long): Double = {
      VDILMStore.addGetValueCalls

      // find the index in the map
      val result =
            map.find(entry => entry._2.contains(index)) match {
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
   def addValueForRepresentation(value: Double, index: Long): VDILMStore = {
      val indicesForValue: List[Long] =
            map.getOrElse(value, null)
      if (indicesForValue != null) {
         map += (value -> (index :: indicesForValue))
      }
      else {
         map += (value -> List(index))
      }

      // return this
      this
   }

   /**
    * adds a new value and the sequence of indexes related to this value
    * @param value new value to add
    * @param indexes indexes storing such value
    */
   def addNonCheckValueForRepresentation(value : Double,
                               indexes : ArrayBuffer[Long]): VDILMStore = {
      // adds the new entry
      map.put(value, indexes.toList)

      // and return this in this case as well
      this
   }

   /**
    * produces a collection of stores: one for each
    * value
    *  @return
    */
   override def split: List[ValueStore] = {
      map.keySet.map(value => {
         VDILMStore(variables, value, map(value))
      }).toList
   }

   /**
    * Gets the complete list of values
    *
    * @return list of values
    */
   override def getListValues: List[Double] = {
      // considers each index and gets its value
      (0L until variables.possibleValues).map(index => {
         getValue(index)
      }).toList
   }

   /**
    * Gets the list of values without repeated values
    *
    * @return list of different values
    */
   override def getDifferentValues : List[Double] = {
      map.keySet.toList.sorted
   }

   /**
    * Gets the indices containing a certain value
    *
    * @param value value of interest
    * @return indices where the value is stored
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // just retrieve the entry for this value
      map.get(value).orNull
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list if indices
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

      // gets the number of indices stored
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
    * @return list of proportions
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters: List[Int] =
         map.keySet.map(value => {
                        map(value).length
         }).toList

      // computes and return the proportions
      // for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets a tuple with number of indices stored,
    * number of values and number of different
    * values
    *
    * @return tuple with information
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val values = map.keySet.size

      // determine the number of indices
      val numberIndices =
            map.map(entry => entry._2.length).sum

      // return the tuple with possible values, number of
      // indices stored in the potential, number of values
      // stored and number of different values (the same
      // as the previous one)
      (numberIndices, values, values)
   }

   /**
    * toString method
    *
    * @return string with information about the object
    */
   override def toString: String = {
      var output = "MapLIMStore (mutable) .......................\n"
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
      VDILMStore.combineDefault)
   registerCombinationFunction(OperatorType.ALT1,
      VDILMStore.combineAlt1)
   registerCombinationFunction(OperatorType.ALT2,
      VDILMStore.combineAlt2)
   registerCombinationFunction(OperatorType.ALT3,
      VDILMStore.combineAlt3)
   registerCombinationFunction(OperatorType.ALT4,
      VDILMStore.combineAlt4)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      VDILMStore.marginalizeDefault)
   registerMarginalizationFunction(OperatorType.ALT1,
      VDILMStore.marginalizeAlt1)
   registerMarginalizationFunction(OperatorType.ALT2,
      VDILMStore.marginalizeAlt2)

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

      // merge both list of indices
      val indices = indices1 ::: indices2

      // modifies the map removing merged entries and adding the new one
      map -= value1
      map -= value2
      map += (newValue -> indices)

      // return this
      this
   }
}

/**
 * Companion object acting as factory
 */
object VDILMStore extends Combiner with Marginalizer{
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls
   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values values of the potential
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): VDILMStore = {
      // sets default value using the associated trait: in
      // this case the default value is obtained with the
      // analysis of the sequence of values
      //val defaultValueComputer =
      //   DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //   defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an
         // empty store
         case Nil =>
            val valueList = List()
            new VDILMStore(variables, mutable.HashMap(0.0 -> valueList))

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

            // For each array of indices (related to
            // the same value) get the corresponding list of
            // indices
            val result: Map[Double, List[Long]] =
            differentValues.indices.map(index => {
               differentValues(index) -> indicesForVal(index).toList
            }).toMap

            // now creates the object storing the values. The
            // final call to seq is required for getting a
            // sequential collection at the end
            new VDILMStore(variables,
                     collection.mutable.Map() ++ result.seq)
      }
   }

   /**
    * Factory method for an empty storage
    *
    * @param variables potential domain
    * @return created object
    */
   def apply(variables: VariableSet): VDILMStore = {
      // just initializes the the map as an empty collection
      new VDILMStore(variables, collection.mutable.Map())
   }

   /**
    * creates a new store for a values and its indexes
    * @param variables target variables
    * @param value value to store
    * @param indices indexed with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, indices : List[Long]): VDILMStore = {
      new VDILMStore(variables, mutable.Map(value -> indices))
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
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // considers all the indexes of result and produces a list
      // of pairs type (double - long) storing the values related
      // to each index in result
      val pairs = (0.toLong until mapper.resultDomain.possibleValues).
         map(index => {
            // produces the pair
            val indexes: (Long, Long) = mapper.mapIndices(index)

            // produces the pair
            (Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2)), index)
         }).toList

      // creates the result
      val result = VDILMStore(mapper.resultDomain)

      // adds all the pairs: all checks are required and this
      // is obtained with addValue
      pairs.foreach(pair => result.addValue(pair._1, pair._2))

      // return result
      result
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   def combineAlt1(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // gets pairs value - index
      val pairs = (0.toLong until mapper.resultDomain.possibleValues).
         par.map(index => {
         // produces the pair
         val indexes: (Long, Long) = mapper.mapIndices(index)
         //println("index in result: " + index + " pairs of indexes: " + indexes)

         // produces the pair
         (Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2)), index)
      }).toList

      // creates the result
      val result = VDILMStore(mapper.resultDomain)

      // adds all the pairs (all checks required)
      pairs.foreach(pair => result.addValue(pair._1, pair._2))

      // return result
      result
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   def combineAlt2(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // gets pairs value - index
      val pairs = (0.toLong until mapper.resultDomain.possibleValues).
         par.map(index => {
         // produces the pair
         val indexes: (Long, Long) = mapper.mapIndices(index)

         // produces the pair
         (Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2)), index)
      })

      // add a parallel filter to remove pairs with 0's
      val filteredPairs = pairs.filter(_._1 != Util.DEFAULTVALUE).toList

      // creates the result
      val result = VDILMStore(mapper.resultDomain)

      // adds all the pairs (now the comparison with 0's is not required)
      // and the method addValueForRepresentation is used directly)
      filteredPairs.foreach(pair =>
         result.addValueForRepresentation(pair._1, pair._2))

      // return result
      result
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   def combineAlt3(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // gets pairs value - index
      val pairs = ArrayBuffer[(Double, Long)]()
      (0.toLong until mapper.resultDomain.possibleValues).
         foreach(index => {
            // produces the pair
            val indexes: (Long, Long) = mapper.mapIndices(index)
            //println("index in result: " + index + " pairs of indexes: " + indexes)
            val val1 = valst1.getValue(indexes._1)
            var val2 = Util.DEFAULTVALUE
            if (val1 != Util.DEFAULTVALUE) {
               val2 = valst2.getValue(indexes._2)
               if (val2 != Util.DEFAULTVALUE) {
                  val pair = (Util.roundNumber(val1 * val2), index)
                  pairs += pair
               }
            }
         })

      // creates the result
      val result = VDILMStore(mapper.resultDomain)

      // adds all the pairs: as default values are filtered, values
      // can be included directly through a call to addValueForRepresentation
      pairs.foreach(pair => result.addValueForRepresentation(pair._1, pair._2))

      // return result
      result
   }

   /**
    * alternative combination method
    * @param valst1 first storage
    * @param valst2 second storage
    * @return result of combination
    */
   def combineAlt4(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // creates the list of triplets with the possible results and
      // the values producing the, and filter those different from
      // default value
      val mapTriplets: ParMap[Double, List[(Double, Double, Double)]] =
      Util.makeTriplets(valst1.getDifferentValues, valst2.getDifferentValues).
         par.filter(_._1 != Util.DEFAULTVALUE)

      // each entry in mapTriples produces a new value for the result
      val results: List[(Double, ArrayBuffer[Long])] =
         mapTriplets.par.map(entry =>
            ValueStore.combineIndices(valst1, valst2, entry)).
            toMap.filter(_._2.nonEmpty).toList

      // creates a new storage as result with empty content
      val result = VDILMStore(mapper.resultDomain)

      // now add all entries to the map: no checks required
      for(entry <- results) result.addNonCheckValueForRepresentation(entry._1, entry._2)

      // finally return result
      result
   }

   /**
    * Marginalization method
    *
    * @param valst    potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    */
   override def marginalizeDefault(valst: ValueStore, variable: Variable): ValueStore = {
      // creates a mapper object for this operation
      val mapper = MarginalizeMapper(variable, valst.variables)

      // for each index in result gets the corresponding
      // values
      val content: Array[Double] =
      (0L until mapper.resultDomain.possibleValues).
         map(index => {
            val value = mapper.mapIndexFromResultToSource(index).
               map(valst.getValue).sum
            Util.roundNumber(value)
         }).toArray

      // creates the result as final step
      val result = VDILMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * marginalization method
    *
    * @param valst potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    */
   def marginalizeAlt1(valst: ValueStore, variable: Variable): ValueStore = {
      // creates a mapper object for this operation
      val mapper = MarginalizeMapper(variable, valst.variables)

      // for each index in result gets the corresponding
      // values
      val content: Array[Double] =
      (0L until mapper.resultDomain.possibleValues).
         map(index => {
            val value = mapper.mapIndexFromResultToSource(index).
               par.map(valst.getValue).sum
            Util.roundNumber(value)
         }).toArray

      // creates the result as final step
      val result = IDSMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * marginalization method
    *
    * @param valst potential to marginalize
    * @param variable variable to remove
    * @return result of marginalization
    */
   def marginalizeAlt2(valst: ValueStore, variable: Variable): ValueStore = {
      // creates a mapper object for this operation
      val mapper = MarginalizeMapper(variable, valst.variables)

      // for each index in result gets the corresponding
      // values
      val content = {
         (0L until mapper.resultDomain.possibleValues).
            par.map(index => {
            (Util.roundNumber(
               mapper.mapIndexFromResultToSource(index).map(valst.getValue).sum),
               index)
         })
      }

      // filter pairs removing default values
      val filtered = content.filter(pair => pair._1 != Util.DEFAULTVALUE).toList

      // creates the result as final step
      val result = IDSMStore(mapper.resultDomain)

      // add entries to storage: no check required for non
      // default values and the value is included with a
      // direct call to addValueForRepresentation (testing
      // if the value were previously inserted)
      for(entry <- filtered)
         result.addValueForRepresentation(Util.roundNumber(entry._1), entry._2)

      // return result
      result
   }
}


