package potential.valueBased

import base.{Variable, VariableSet}
import mapper.{CombineMapper, MarginalizeMapper}
import potential.{ValueStoreTypes, _}
import utils.{DataSizes, Util}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Class for storing values with the following structure:
 * dictionary with pairs (value - set of mutable indices
 * where the value is stored)
 * @param variables potential domain
 * @param map dictionary with the information
 */
case class VDISMStore(variables: VariableSet,
                      map: mutable.Map[Double, Set[Long]])
                     extends ValueDrivenStore with Combiner with Marginalizer {
   /**
    * default value strategy
    */
   //val defaultValueComputer =
   //         DefaultValueComputer(defaultValueComputerType)

   /**
    * kind of store
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.VDISETMUT

   /**
    * Gets the value for a corresponding index
    *
    * @param index target index
    * @return value stored in the index passed as argument
    */
   def getValue(index: Long): Double = {
      // find the index in the map
      map.find(entry => entry._2.contains(index)).
         getOrElse((Util.DEFAULTVALUE, Set()))._1
   }

   /**
    * adds a new value to the map
    *
    * @param value target value
    * @param index index where the value is stored
    * @return result of the operation
    */
   def addValueForRepresentation(value: Double, index: Long): VDISMStore = {

      val indicesForValue: Set[Long] =
                  map.getOrElse(value, null)
      if (indicesForValue != null) {
         map += (value -> (indicesForValue + index))
      }
      else {
         map += (value -> Set(index))
      }

      // return this
      this
   }

   /**
    * produces a collection of stores: one for each
    * value
    *  @return
    */
   override def split: List[ValueStore] = {
      map.keySet.map(value => {
         VDISMStore(variables, value, map(value))
      }).toList
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
    * @param value target value
    * @return list of indices where the value is stored
    */
   def getIndicesForValue(value: Double): List[Long] = {
      // just retrieve the entry for this value
      map(value).toList
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

      // gets the number of indices represented with
      // grains
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
    * @return proportions of values
    */
   override def getValuesProportions: List[Double] = {
      val concreteValues = getListValues

      // gets the counter for each value
      val counters: List[Int] = map.keySet.map(value => {
         map(value).size
      }).toList

      // computes and return the proportions for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
    * Gets the size of the store, as a tuple with number
    * of indices stored, number of values stored and number
    * of different values
    *
    * @return
    */
   override def getSize: (Long, Long, Long) = {
      // determine the number of values
      val values = map.keySet.size

      // determine the number of indices
      val numberIndices = map.map(entry => entry._2.size).sum

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
      var output = "MapSIMStore (immutable).......................\n"
      output = output + " Type: " + kind + "\n"
      //output = output + " Def. value treatment: " +
      //                           defaultValueComputer.getType + "\n"
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
      VDISMStore.combineDefault)
   registerCombinationFunction(OperatorType.ALT1,
      VDISMStore.combineAlt1)
   registerCombinationFunction(OperatorType.ALT2,
      VDISMStore.combineAlt2)
   registerCombinationFunction(OperatorType.ALT3,
      VDISMStore.combineAlt3)
   registerCombinationFunction(OperatorType.ALT4,
      VDISMStore.combineAlt4)
   registerCombinationFunction(OperatorType.ALT5,
      VDISMStore.combineAlt5)
   registerMarginalizationFunction(OperatorType.DEFAULT,
      VDISMStore.marginalizeDefault)
   registerMarginalizationFunction(OperatorType.ALT1,
      VDISMStore.marginalizeAlt1)
   registerMarginalizationFunction(OperatorType.ALT2,
      VDISMStore.marginalizeAlt2)
}

/**
 * Companion object acting as factory
 */
object VDISMStore extends Combiner with Marginalizer{
   /**
    * factory method
    *
    * @param variables variables of the potential
    * @param values values of the potential
    * @return
    */
   def apply(variables: VariableSet, values: Array[Double]): VDISMStore = {
      // sets default value using the associated trait: in
      // this case the default value is obtained with the
      // analysis of the sequence of values
      //val defaultValueComputer =
      //      DefaultValueComputer(defaultValueComputerType)
      //val defaultValue: Double =
      //   defaultValueComputer.computeDefaultValue(values);

      // consider the list of variables
      variables.variableList match {
         // if no variables specified just creates an empty
         // store
         case Nil =>
            val valuesSet: Set[Long] = Set()
            new VDISMStore(variables, mutable.HashMap(0.0 -> valuesSet))

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
            val result =
                  collection.mutable.Map() ++
                     differentValues.indices.map(index => {
               differentValues(index) -> indicesForVal(index).toSet
            }).toMap

            // now creates the object storing the values. The
            // final call to seq is required for getting a
            // sequential collection at the end
            new VDISMStore(variables, result.seq)
      }
   }

   /**
    * direct creation of object from domain and map of values-indexes
    * @param variables variable to include in domain
    * @param values map of entries double - set of related indexes
    * @return new object
    */
   def apply(variables: VariableSet, values: mutable.Map[Double, Set[Long]]): VDISMStore = {
      new VDISMStore(variables, values)
   }

   /**
    * factory method for creating a store prepared for adding new values.
    * That is, it starts with empty content. default value is defined
    * from creation
    * @param variables domain of storage
    * @return empty storage
    */
   def apply(variables: VariableSet): VDISMStore = {
      // in any case creates an empty storage
      VDISMStore(variables, Array[Double]())
   }

   /**
    * creates a new store for a values and its indexes
    * @param variables target variables
    * @param value value to store
    * @param indices indexed with value assigned
    * @return
    */
   def apply(variables : VariableSet, value : Double, indices : Set[Long]): VDISMStore = {
      new VDISMStore(variables, mutable.Map(value -> indices))
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
      val pairs = (0.toLong until mapper.resultDomain.possibleValues).
         map(index => {
            // produces the pair
            val indexes: (Long, Long) = mapper.mapIndices(index)

            // produces the pair
            (Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2)), index)
         }).toList

      // creates the result
      val result = VDISMStore(mapper.resultDomain)

      // adds all the pairs: addValue is required to discard
      // default value (addValue) and if the value is already
      // included (posterior call of addValueForRepresentation
      // made from addValue)
      pairs.foreach(pair => {
         result.addValue(pair._1, pair._2)
      })

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

         // produces the pair
         (Util.roundNumber(valst1.getValue(indexes._1) * valst2.getValue(indexes._2)), index)
      }).toList

      // creates the result
      val result = VDISMStore(mapper.resultDomain)

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
      val result = VDISMStore(mapper.resultDomain)

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
      val values = mutable.Map[Double, Set[Long]]()
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

                  // add the value to values map
                  val entryForValue = values.getOrElse(pair._1, null)
                  if(entryForValue == null){
                     values += (pair._1 -> Set(pair._2))
                  }
                  else{
                     values += (pair._1 -> (entryForValue + pair._2))
                  }
               }
            }
         })

      // creates the result
      val result = VDISMStore(mapper.resultDomain, values)

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
      val groupedPairs = {
         pairs.groupBy(_._1).mapValues(_.map(_._2).toSet)
      }

      // convert immutable map to mutable map
      val mutableMap = mutable.Map[Double, Set[Long]]()
      mutableMap ++= groupedPairs


      // creates the result
      val result = VDISMStore(mapper.resultDomain, mutableMap)

      // return result
      result
   }

   /**
    * alternative combination method
    * @param valst1 first storage
    * @param valst2 second storage
    * @return result of combination
    */
   def combineAlt5(valst1: ValueStore, valst2: ValueStore): ValueStore = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // creates the list of triplets with the possible results and
      // the values producing them, and filter those different from
      // default value
      val mapTriplets: Map[Double, List[(Double, Double, Double)]] =
      Util.makeTriplets(valst1.getDifferentValues, valst2.getDifferentValues).
         filter(_._1 != Util.DEFAULTVALUE)

      // each entry in mapTriples produces a new value for the result
      val results = mapTriplets.map(entry =>
         ValueStore.combineIndices(valst1, valst2, entry)).
                                    filter(_._2.nonEmpty)

      // convert to mutable map
      val mutableMap = mutable.Map[Double, Set[Long]]()
      for(entry <- results){
         mutableMap += (entry._1 -> entry._2.toSet)
      }

      // creates a new storage as result with empty content
      val result = VDISMStore(mapper.resultDomain, mutableMap)

      // finally return result
      result
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
      val result = VDISMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * Marginalization method (alt1): parallelize the sum of
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
      val result = VDISMStore(mapper.resultDomain, content)

      // return result
      result
   }

   /**
    * Marginalization method (alt1): parallelize the sum of
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
      val result = VDISMStore(mapper.resultDomain)

      // now add all entries to the map: only check the previous
      // inclusion of value into the collection
      for(entry <- filtered)
         result.addValueForRepresentation(entry._1, entry._2)

      // return result
      result
   }
}











