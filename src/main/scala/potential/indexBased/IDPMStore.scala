package potential.indexBased

import base.{Variable, VariableSet}
import mapper.{CombineMapper, MarginalizeMapper}
import potential._
import utils.{DataSizes, Util}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ParMap

/**
 * Class for storing values as a value driven store with
 * the following structure: different values are stored
 * in a mutable array; another array contains pairs with
 * information about index of potential - index in array
 * of values
 * @constructor creates a new object using the information
 *              passed as argument
 * @param variables domain of the store
 * @param indices arrays storing indices (with values) and
 *                indices of the corresponding values in
 *                values array
 * @param values array of values stored in the potential
 */
case class IDPMStore(variables: VariableSet,
                     indices: ArrayBuffer[(Long, Long)],
                     values: ArrayBuffer[Double]) extends ValueDrivenStore
              with Combiner with Marginalizer{

   /**
    * sets behavior for default value treatment
    */
   //val defaultValueComputer =
   //   DefaultValueComputer(defaultValueComputerType)

   /**
    * sets the kind of store
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.IDPMSTORE

   /**
    * Gets the value for a corresponding index
    *
    * @param index target index
    * @return value corresponding to index
    */
   def  getValue(index: Long): Double = {
      // increments counter calls
      IDPMStore.addGetValueCalls

      // checks the array of indices looking for a pair
      // containing the index as first element
      //val result: (Long, Long) = {
      //indices.find(pair => pair._1 == index).
      //                              getOrElse((index, -1))
      //}

      // compose final result in order to return defaultValue
      // if the index was not found. This is because 0's are
      // not stored
      //if (result._2 == -1) Util.DEFAULTVALUE
      //else values(result._2.toInt)

      var result = Util.DEFAULTVALUE
      for(i <- indices.indices if indices(i)._1 <= index){
         if(indices(i)._1 == index) result=values(indices(i)._2.toInt)
      }
      result

      //print(indices)
      //val result = indices.indexOf(index)
      //if(result == -1) Util.DEFAULTVALUE
      //else values(result)
   }

   def sortIndices() : Unit = {
      indices.sortBy(u => u._1)
   }

   /**
    * adds a value to the store
    *
    * @param value new value to store
    * @param index index where the value is stored
    */
   def addValueForRepresentation(value: Double, index: Long): IDPMStore = {
      // checks if the value is already contained
      // into values
      val indexForValue =
         values.indices.find(index => {
            //values(index) == value
            Util.nearEqual(value, values(index))
         }).getOrElse(-1)

      // if the value is already defined, add a new entry to
      // indices. In any other case it is needed to add a
      // new value
      if (indexForValue != -1) {
         indices += ((index, indexForValue.toLong))
      }
      else {
         // add a new index on indices array
         indices += ((index, values.length.toLong))

         // add the new value
         values += value
      }

      // return this (mutable data structure)
      this
   }

   /**
    * adds a new value and the sequence of indexes related to this value
    * @param value new value to add
    * @param indexes indexes storing such value
    */
   def addValueForRepresentation(value : Double, indexes : ArrayBuffer[Long]): IDPMStore = {
      // gets the last index of values: this will be the position for
      // this new value
      val lastIndex = values.size

      // checks if the new value is the default one
      if(Util.nearEqual(value, Util.DEFAULTVALUE)) this
      else {
         // adds the value
         values += value

         // adds all the indexes
         indexes.foreach(index => indices += ((index, lastIndex)))

         // and return this in this case as well
         this
      }
   }

   /**
    * adds a new value and the sequence of indexes related to
    * this value without checking non default values not the
    * previous presence of the value in the collection
    * @param value new value to add
    * @param indexes indexes storing such value
    */
   def addNonCheckValueForRepresentation(value : Double, indexes : ArrayBuffer[Long]): IDPMStore = {
      // gets the last index of values: this will be the position for
      // this new value
      val lastIndex = values.size

      // adds the value
      values += value

      // adds all the indexes
      indexes.foreach(index => indices += ((index, lastIndex)))

      // and return this in this case as well
      this
   }

   /**
    * split the store into a new list of stores, one per value
    *  @return
    */
   override def split: List[ValueStore] = {
      (0L until values.length).map(index => {
         val indexesValue = indices.filter(_._2 == index).map(_._2)
         IDPMStore(variables, values(index.toInt), indexesValue)
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
    * @return list of indices
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // gets the indices related to a certain value
      val valueIndex =
         values.indices.
            find(index => values(index) == value).getOrElse(-1)

      // now gets the indices pointing to this value
      val result = if (valueIndex != -1) {
         indices.filter(pairs => pairs._2 == valueIndex).
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
      indices.map(entry => entry._1).toList
   }

   /**
    * Gets the proportion of zeros
    *
    * @return computed proportion
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
    * @return computed proportions
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters = values.map(value => {
         indices.map(pair => pair._2).
            map(indexInValues =>
               values(indexInValues.toInt) == value).length
      })

      // computes and return the proportions for
      // each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      }).toList
   }

   /**
    * Gets the kind of the store
    *
    * @return tuple with possible indices, number of values
    *         and number of values
    */
   override def getSize: (Long, Long, Long) = {
      // return a tuple with number of indices stored and
      // number of values stored
      (indices.length, values.length, values.length)
   }

   /**
    * toString method
    *
    * @return string with object information
    */
   override def toString: String = {
      var output = "ArrayIMStore (mutable store).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                                 defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + indices.mkString("\n") + "\n"
      output = output + "Different values: " +
                                    values.mkString(" ") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + "\n"
      val size = getSize
      output = output + "indices stored: " + size._1 + "\n"
      output = output + "values stored: " + size._2 + "\n"
      output = output + "number of different values: " + size._3 + "\n"
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
      //val variableSizes = variables.getObjectSize
      val variableSizes = variables.getMemorySize

      // get size of indices
      val indicesSizes =
         indices.length * DataSizes.LONG * 2

      // get size of values; adds one due to default
      // value
      val valueSizes =
         values.length * DataSizes.DOUBLE + DataSizes.DOUBLE

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
      IDPMStore.combineDefault)
   registerCombinationFunction(OperatorType.ALT1,
      IDPMStore.combineAlt1)
   registerCombinationFunction(OperatorType.ALT2,
      IDPMStore.combineAlt2)
   registerCombinationFunction(OperatorType.ALT3,
      IDPMStore.combineAlt3)
   registerCombinationFunction(OperatorType.ALT4,
      IDPMStore.combineAlt4)
   registerCombinationFunction(OperatorType.ALT5,
      IDPMStore.combineAlt5)
   registerCombinationFunction(OperatorType.ALT6,
      IDPMStore.combineAlt6)
   registerCombinationFunction(OperatorType.ALT7,
      IDPMStore.combineAlt7)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      IDPMStore.marginalizeDefault)
   registerMarginalizationFunction(OperatorType.ALT1,
      IDPMStore.marginalizeAlt1)
   registerMarginalizationFunction(OperatorType.ALT2,
      IDPMStore.marginalizeAlt2)
   registerMarginalizationFunction(OperatorType.ALT3,
      IDPMStore.marginalizeAlt3)

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

      // creates a new list of values
      var newValues = values.clone
      newValues -= value1 -= value2 += newValue
      newValues = newValues.sorted

      // gets the new positions for the map linking indices in
      // potentials and indices in array of values
      val newIndices = indices.map(entry => {
         // gets the value for the index
         val value = values(entry._2.toInt)
         // gets the new index in newValues
         val newIndex = newValues.indexWhere(_ == value, 0)
         // return the new entry
         if(newIndex != -1)
            (entry._1, newIndex.toLong)
         else
            (entry._1, newValues.indexWhere(_ == newValue, 0).toLong)
      })

      // change previous values and indices by new arrays
      values.clear
      values ++= newValues
      indices.clear
      indices ++= newIndices

      // return this
      this
   }
}

/** ********************************************************* */

/**
 * Companion object
 */
object IDPMStore extends Combiner with Marginalizer {
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
    * Factory method
    *
    * @param variables potential domain
    * @param values values stored
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): IDPMStore = {
      // gets default value
      //val defaultValueComputer =
      //         DefaultValueComputer(defaultValueComputerType)
      //val defaultValue =
      //         defaultValueComputer.computeDefaultValue(values)

      // consider the list of variables
      val result = variables.variableList match {
         // if no variables specified just creates an
         // empty store
         case Nil =>
            new IDPMStore(variables, ArrayBuffer(), ArrayBuffer(0))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: Array[Double] =
               values. filter(value => value != Util.DEFAULTVALUE).
                  distinct

            // stores the indices for each value
            val arrayIndices = ArrayBuffer[(Long, Long)]()
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
            val newStore = new IDPMStore(variables, arrayIndices,
               ArrayBuffer() ++ differentValues)

            // return newStore
            newStore
      }
      result
   }

   /**
    * factory method for creating a store prepared for adding new values.
    * That is, it starts with empty content. default value is defined
    * from creation
    * @param variables domain of storage
    * @return empty storage
    */
   def apply(variables: VariableSet): IDPMStore = {
      // in any case creates an empty storage
      new IDPMStore(variables, ArrayBuffer(), ArrayBuffer())
   }

   /**
    * Creates a store for a single value and its indexes
    * @param variables target variables for the store
    * @param value single value for the store
    * @param indices list of indices for the value
    * @return
    */
   def apply(variables : VariableSet, value : Double,
             indices : ArrayBuffer[Long]) = {
      val pairs = ArrayBuffer(indices.map((_, 0L)).toArray : _*)
      new IDPMStore(variables, pairs, ArrayBuffer(value))
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   override def combineDefault(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // considers all the indexes of result and produces a list
      // of pairs type (double - long) storing the values related
      // to each index in result
      val content = (0.toLong until mapper.resultDomain.possibleValues).
         map(index => {
            // produces the pair
            val indexes: (Long, Long) = mapper.mapIndices2(index)
            //println("index in result: " + index + " pairs of indexes: " + indexes)

            // produces the value
            Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2))
      }).toArray

      // creates the result
      val result = IDPMStore(mapper.resultDomain, content)

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
      val result = IDPMStore(mapper.resultDomain)

      // adds all the pairs with addValue, testing values to
      // store are not the default value and checking if they
      // were previously added to the collection (posterior call
      // to addValueForRepresentation) made from addValue
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
      val result = IDPMStore(mapper.resultDomain)

      // adds all the pairs (now the comparison with 0's is not required)
      // and the method addValueForRepresentation is used directly. It is
      // needed to check if the value were previously stored
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
      val result = new IDPMStore(mapper.resultDomain, ArrayBuffer(),
         ArrayBuffer())

      // adds all the pairs. No required to check equality to
      // default value but the previous inclusion of the value
      // in the collection
      pairs.foreach(pair =>
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
   def combineAlt4(valst1: ValueStore, valst2: ValueStore): ValueStore = {
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

      // group values and created pairs of type Double - Array of Long
      // with the corresponding indexes
      val groupedPairs =
         pairs.groupBy(_._1).mapValues(_.map(_._2)).toSeq

      // creates the result
      val result = IDPMStore(mapper.resultDomain)

      // adds all the pairs. No required to check equality to
      // default value but the previous inclusion of the value
      // in the collection
      groupedPairs.foreach(pair =>
         result.addNonCheckValueForRepresentation(pair._1, pair._2))

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
   def combineAlt5(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // gets pairs value - index
      val pairs = ArrayBuffer[(Double, Long)]()
      (0.toLong until mapper.resultDomain.possibleValues).
         foreach(index => {
            // get the index in the first store
            val index1 = mapper.mapToOperand1(index)

            // gets the value for this index
            val val1 = valst1.getValue(index1)

            // if needed, gets the index in the second store
            if(val1 != Util.DEFAULTVALUE) {
               val index2 = mapper.mapToOperand2(index)
               val val2 = valst2.getValue(index2)

               // now store the result if required
               if(val2 != Util.DEFAULTVALUE){
                  val pair = (Util.roundNumber(val1 * val2), index)
                  pairs += pair
               }
            }
         })

      // group values and created pairs of type Double - Array of Long
      // with the corresponding indexes
      val groupedPairs =
      pairs.groupBy(_._1).mapValues(_.map(_._2)).toSeq

      // creates the result
      val result = IDPMStore(mapper.resultDomain)

      // adds all the pairs. No required to check equality to
      // default value but the previous inclusion of the value
      // in the collection
      groupedPairs.foreach(pair =>
         result.addNonCheckValueForRepresentation(pair._1, pair._2))

      // return result
      result
   }

   /**
    * alternative combination method
    * @param valst1 first storage
    * @param valst2 second storage
    * @return result of combination
    */
   def combineAlt6(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // creates the list of triplets with the possible results and
      // the values producing them, and filter those different from
      // default value
      val mapTriplets: ParMap[Double, List[(Double, Double, Double)]] =
         Util.makeTriplets(valst1.getDifferentValues, valst2.getDifferentValues).
                              par.filter(_._1 != Util.DEFAULTVALUE)

      // each entry in mapTriples produces a new value for the result
      // NOTE: use of parallel facility
      val results: List[(Double, ArrayBuffer[Long])] = mapTriplets.par.map(entry =>
         ValueStore.combineIndices(valst1, valst2, entry)).toMap.filter(_._2.nonEmpty).toList

      // creates a new storage as result with empty content
      val result = IDPMStore(mapper.resultDomain)

      // now add all entries to the map: non required any previous
      // check (non default values are discarded and values will not
      // be repeated)
      for(entry <- results) result.addNonCheckValueForRepresentation(entry._1, entry._2)

      // finally return result
      result
   }

   /**
    * Combination method
    *
    * @param valst1 first potential to combine
    * @param valst2 second potential to combine
    * @return result of combination
    */
   def combineAlt7(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // considers all the indexes of result and produces a list
      // of pairs type (double - long) storing the values related
      // to each index in result
      val pairs = ArrayBuffer[(Double, Long)]()
      (0.toLong until mapper.resultDomain.possibleValues).foreach(index => {
            // gets the index and value for for valst1
            val index1 = mapper.mapToOperand1(index)
            val val1 = valst1.getValue(index1)

            if(val1 != Util.DEFAULTVALUE){
               // gets index and value for valst2
               val index2 = mapper.mapToOperand2(index)
               val val2 = valst2.getValue(index2)

               if(val2 != Util.DEFAULTVALUE){
                  // produce the pair
                  val pair = (Util.roundNumber(val1 * val2), index)
                  pairs += pair
               }
            }
         })

      // creates the result
      val result = IDPMStore(mapper.resultDomain)

      // adds all the pairs: addValue is required to discard
      // default value (addValue) and if the value is already
      // included (posterior call of addValueForRepresentation
      // made from addValue)
      pairs.foreach(pair => result.addValueForRepresentation(pair._1, pair._2))

      // return result
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
               Util.roundNumber(mapper.mapIndexFromResultToSource(index).
                  map(valst.getValue).sum)
            }).toArray

      // creates the result as final step
      val result = IDPMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * Marginalization method (alt1): parallelize the summation of
    * the configurations related to the same index in the result
    *
    * @param valst    potential to marginalize
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
      val result = IDPMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * Marginalization method (alt1): parallelize the summation of
    * the configurations related to the same index in the result
    *
    * @param valst    potential to marginalize
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

      // now filter all the pairs with 0s
      val filtered = content.filter(pair => pair._1 != Util.DEFAULTVALUE).toList

      // creates the result as final step
      val result = IDPMStore(mapper.resultDomain)

      // now add all entries to the map: only check the previous
      // inclusion of value into the collection
      for(entry <- filtered)
         result.addValueForRepresentation(entry._1, entry._2)

      // return result
      result
   }

   /**
    * marginalization operation
    * @param valst potential to marginalize
    * @param variable variable to discard
    * @return result potential
    */
   def marginalizeAlt3(valst : ValueStore, variable : Variable) : ValueStore = {
      // create a mapper object for this operation
      val mapper = MarginalizeMapper(variable, valst.variables)

      // group indexes according to their projection into result domain
      val groups = valst.getIndices.groupBy(mapper.mapIndexFromSourceToResult)

      // for each group gets the destination index and the value
      val content = groups.map(entry => {
         // gets values for all indexes in entry-value
         (Util.roundNumber(entry._2.map(valst.getValue).sum), entry._1)
      })

      // creates the result as final step
      val result = IDPMStore(mapper.resultDomain)

      // include non zero values to result
      for(entry <- content if entry._1 != Util.DEFAULTVALUE) {
         result.addValueForRepresentation(entry._1, entry._2)
      }

      // return result
      result
   }
}
