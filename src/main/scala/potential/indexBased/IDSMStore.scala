package potential.indexBased

import base.{Variable, VariableSet}
import mapper.{CombineMapper, MarginalizeMapper}
import potential._
import utils.{DataSizes, Util}

import scala.collection.Set
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ParMap

/**
 * Class for storing values with the following structure:
 * values are stored in an array; each value is paired with
 * the same position in another array where indices are
 * stored in sets. In this case mutable arrays are employed
 * @constructor creates a new instance using the data
 *              passed as argument
 * @param variables domain of the related potential
 * @param indexesSet array of sets of indices
 * @param values array of values
 */
case class IDSMStore(variables: VariableSet,
                     indexesSet: ArrayBuffer[Set[Long]],
                     values: ArrayBuffer[Double]) extends ValueDrivenStore
                         with Combiner with Marginalizer {

   /**
    * default value behavior
    */
   //val defaultValueComputer = DefaultValueComputer(defaultValueComputerType)

   /**
    * Gets the value for a given index
    *
    * @param index index of interest
    * @return value for index
    */
   def getValue(index: Long): Double = {
      // find the index in the sets
      val indexResult = indexesSet.indices.
         find(indexInArray => indexesSet(indexInArray).
               contains(index)).getOrElse(-1)

      // return the value stored in the corresponding
      // index
      val result = if (indexResult != -1) values(indexResult)
                  else Util.DEFAULTVALUE

      // return result
      result
   }

   /**
    * adds a value to the store. As arrays are mutable, the
    * same object is returned at the end
    *
    * @param value value to add
    * @param index index where the value must be stored
    */
   def addValueForRepresentation(value: Double, index: Long): IDSMStore = {
      // checks if the value is already contained into
      // values
      val indexForValue =
         values.indices.
            find(index => values(index) == value).getOrElse(-1)

      // if the value is already defined, add a new entry
      // to indices. In any other case it is needed to add
      // a new value
      if (indexForValue != -1) indexesSet(indexForValue) += index
      else {
         // creates a new set for the the value
         indexesSet += Set(index)
         values += value
      }

      // return this
      this
   }

   /**
    * adds a new value and the sequence of indexes related to this value
    * @param value new value to add
    * @param indexes indexes storing such value
    */
   def addNonCheckValueForRepresentation(value : Double, indexes : ArrayBuffer[Long]): IDSMStore = {
      // adds the value
      values += value

      // adds the set of indexes
      indexesSet += indexes.toSet

      // and return this in this case as well
      this
   }

   /**
    * split method for generating a store for each value
    *  @return
    */
   override def split: List[ValueStore] = {
      values.indices.map(index => {
         IDSMStore(variables, values(index), indexesSet(index))
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
    * @return list of indices where the value is stored
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // gets the indices related to a certain value
      val valueIndex = values.indices.
         find(index => values(index) == value).getOrElse(-1)

      // now gets the indices pointing to this value
      val result = if (valueIndex != -1) {
         indexesSet(valueIndex)
      }
      else {
         Set()
      }

      // return result converted to List
      result.toList
   }

   /**
    * Gets the list of indices defined for the store
    *
    * @return list of indices stored
    */
   override def getIndices: List[Long] = {
      indexesSet.flatMap(set => set.toList).toList
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

      // gets the number of indices related to values
      // different from 0
      val numberOfIndices =
               indexesSet.map(set => set.size).sum

      // the complement is the number of 0's
      val numberZeros = size - numberOfIndices

      // return the desired proportion
      numberZeros / size
   }

   /**
    * gets the proportions of repetitions for each value
    *
    * @return porportions for each one of the set of
    *         different values
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter of indices for each value
      val counters = (1 until values.length).map(
         index => indexesSet(index).size
      )

      // computes and return the proportions for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      }).toList
   }

   /**
    * gets a tuple with information about indices stored,
    * values stored and number of different values (these
    * two values are the same for this storage)
    *
    * @return
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of indices
      val numberIndices =
                  indexesSet.map(set => set.size).sum

      // return the tuple with possible values, number of
      // indices stored in the potential, number of values
      // stored and number of different values (the same
      // as the previous one)
      (numberIndices, values.length, values.length)
   }

   /**
    * toString method
    *
    * @return string with information about the object
    */
   override def toString: String = {
      var output = "ArraySIMStore (mutable store).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                        defaultValueComputer.getType + "\n"
      output = output + variables.toString
      output = output + "Main variable: " + mainVariable.name
      output = output + "\nIndices and value indices: \n"
      output = output + indexesSet.mkString("\n") + "\n"
      output = output + "Different values: " + values.mkString(" ") + "\n"
      output = output + "Default value: " + Util.DEFAULTVALUE + "\n"
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
    * @return memory size estimation
    */
   def getMemorySize: Long = {

      // get size due to variables
      //val variableSizes = variables.getObjectSize
      val variableSizes = variables.getMemorySize

      // get size of indices
      val indicesSizes =
               indexesSet.map(set => set.size).sum * DataSizes.LONG

      // get size of values: adds one to default value
      val valueSizes =
               values.length * DataSizes.DOUBLE + +DataSizes.DOUBLE

      // considers the size of both arrays and a number
      // of sets equals to the length of the arrays
      val structuresSize =
            DataSizes.ARRAY * 2 + DataSizes.SET * indexesSet.length

      // return the global sum
      variableSizes + indicesSizes + valueSizes + structuresSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
      IDSMStore.combineDefault)
   registerCombinationFunction(OperatorType.ALT1,
      IDSMStore.combineAlt1)
   registerCombinationFunction(OperatorType.ALT2,
      IDSMStore.combineAlt2)
   registerCombinationFunction(OperatorType.ALT3,
      IDSMStore.combineAlt3)
   registerCombinationFunction(OperatorType.ALT4,
      IDSMStore.combineAlt4)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      IDSMStore.marginalizeDefault)
   registerMarginalizationFunction(OperatorType.ALT1,
      IDSMStore.marginalizeAlt1)
   registerMarginalizationFunction(OperatorType.ALT2,
      IDSMStore.marginalizeAlt2)
   registerMarginalizationFunction(OperatorType.ALT3,
      IDSMStore.marginalizeAlt3)
}

/**
 * Companion object offering factory methods
 */
object IDSMStore extends Combiner with Marginalizer {

   /**
    * Factory method
    *
    * @param variables domain of potential
    * @param values values of potential
    * @return created object
    */
   def apply(variables: VariableSet, values: Array[Double]): IDSMStore = {
      // sets default value using the associated trait: in
      // this case the default value is obtained with the
      // analysis of the sequence of values
      //val defaultValueComputer =
      //         DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //         defaultValueComputer.computeDefaultValue(values.toArray);

      // consider the list of variables
      val result = variables.variableList match {
         // if no variables specified just creates an empty store
         case Nil =>
            new IDSMStore(variables, ArrayBuffer(Set[Long]()), ArrayBuffer(0))

         // creates a complete store
         case _ =>
            // get the list of different values
            val differentValues: ArrayBuffer[Double] =
                     new ArrayBuffer[Double]() ++
                     values.filter(value => value != Util.DEFAULTVALUE).distinct

            // gets the indices for each value
            val indicesForVals: ArrayBuffer[Set[Long]] =
               differentValues.map(value => {
                  (0.toLong until values.length.toLong).
                     filter(index => values(index.toInt) == value).toSet
               })

            // now creates the object storing the values.
            val newStore = new IDSMStore(variables, indicesForVals,
                     differentValues)

            // return newStore
            newStore
      }
      result
   }

   /**
    * factory method for an empty storage
    * @param variables domain of storage
    * @return
    */
   def apply(variables : VariableSet) : IDSMStore = {
      new IDSMStore(variables, ArrayBuffer(), ArrayBuffer())
   }

   /**
    * creates a store for a single values and its corresponding
    * indexes
    * @param variables target variables for the store
    * @param value single value for store
    * @param indexes set of indices with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double,
             indexes : Set[Long]): IDSMStore = {
      new IDSMStore(variables, ArrayBuffer(indexes), ArrayBuffer(value))
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

      // gets the pairs of type (value - index) for each index in
      // the result
      val pairs = (0L until mapper.resultDomain.possibleValues).
         map(index => {
            val indexes = mapper.mapIndices(index)

            // produces now the pair
            (Util.roundNumber(valst1.getValue(indexes._1)*valst2.getValue(indexes._2)), index)
         }).toList

      // creates the result
      val result = IDSMStore(mapper.resultDomain)

      // adds all the pairs: add value compares with default
      // value and a call to addValueForRepresentation (from
      // addValue) compares if the value were previously included
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

      // gets the pairs of type (value - index) for each index in
      // the result
      val pairs = (0L until mapper.resultDomain.possibleValues).
         par.map(index => {
            val indexes = mapper.mapIndices(index)

            // produces now the pair
            (Util.roundNumber(valst1.getValue(indexes._1)*valst2.getValue(indexes._2)), index)
         }).toList

      // creates the result
      val result = IDSMStore(mapper.resultDomain)

      // adds all the pairs: required comparison with default value
      // (in addValue) and repetitions in collection (addValueForRepresentation,
      // made from addValue)
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

      // gets the pairs of type (value - index) for each index in
      // the result
      val pairs = (0L until mapper.resultDomain.possibleValues).
         par.map(index => {
         val indexes = mapper.mapIndices(index)

         // produces now the pair
         (Util.roundNumber(valst1.getValue(indexes._1)*valst2.getValue(indexes._2)), index)
      }).toList

      // adds a parallel filter for removing default values
      // (pairs is a parallel collection)
      val filteredPairs = pairs.filter(_._1 != Util.DEFAULTVALUE)

      // creates the result
      val result = IDSMStore(mapper.resultDomain)

      // adds all the pairs: as default value are excluded
      // pairs can be included with a direct call to
      // addValueForRepresentation
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
      val result = IDSMStore(mapper.resultDomain)

      // adds all the pairs: no required check of default values
      // and pairs are included with a direct call to addValueForRepresentation
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
      val results: List[(Double, ArrayBuffer[Long])] = mapTriplets.par.map(entry =>
         ValueStore.combineIndices(valst1, valst2, entry)).toMap.filter(_._2.nonEmpty).toList

      // creates a new storage as result with empty content
      val result = IDSMStore(mapper.resultDomain)

      // now add all entries to the map: the collection of indexes
      // can be included without any previous check
      for(entry <- results) result.addNonCheckValueForRepresentation(entry._1, entry._2)

      // finally return result
      result
   }

   /**
    * marginalization method
    *
    * @param valst potential to marginalize
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
      val result = IDSMStore(mapper.resultDomain)

      // include non zero values to result
      for(entry <- content if entry._1 != Util.DEFAULTVALUE) {
         result.addValueForRepresentation(entry._1, entry._2)
      }

      // return result
      result
   }
}




