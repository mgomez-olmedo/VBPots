package potential

import base.{Configuration, Variable, VariableSet}
import mapper.CombineMapper
import potential.OperatorType.OperatorType
import potential.indexBased.{IDMMStore, IDPIStore, IDPMStore, IDSIStore, IDSMStore}
import potential.valueBased.{VDGLStore, VDGSStore, VDILIStore, VDILMStore, VDISIStore, VDISMStore}
import utils._

import scala.collection.mutable.ArrayBuffer

/**
  * Trait defining the operations to be implemented in ValueStore
  * objects
  */
trait ValueStore extends Combiner with Marginalizer with Serializable {
   /**
     * Data member for storing variables
     */
   val variables: VariableSet

   /**
     * Information about main var and conditioning of the potential
     * owning this store
     */
   val mainVariable: Variable = variables.last
   val conditioningVars: Iterable[Variable] = variables.dropRight(1)

   /**
     * By default, the type is table
     */
   val kind: ValueStoreTypes.Value = ValueStoreTypes.TABLE

   /**
     * Return the variables of the store
     *
     * @return
     */
   def getVariables: VariableSet = {
      PerformanceStats.getVariablesCalls = PerformanceStats.getVariablesCalls + 1

      // return variables
      variables
   }

   /**
     * Returns tht list of indices defined for the store
     *
     * @return
     */
   def getIndices: List[Long] = {
      (0L until variables.possibleValues).toList
   }

   /**
     * Just return a concrete value corresponding to the index
     * passed as argument
     *
     * @param index target index
     * @return
     */
   def getValue(index: Long): Double

   /**
    * Just return a concrete value corresponding to a configuration
    *
    * @param conf target configuration
    * @return
    */
   def getValue(conf: Configuration): Double = {
      // just compute the index
      getValue(conf.computeIndex)
   }

   /**
     * Get the complete list of values
     *
     * @return
     */
   def getListValues: List[Double]

   /**
     * Get the list of different values
     *
     * @return
     */
   def getDifferentValues: List[Double]

   /**
     * Gets the indices related to a value
     *
     * @param value target value
     * @return
     */
   def getIndicesForValue(value: Double): List[Long]

   /**
     * Gets the proportion of zeros
     *
     * @return
     */
   def getZerosProportion: Double = {
      val concreteValues = getListValues
      (concreteValues.count(_ == 0.0) * 100.0) / concreteValues.length
   }

   /**
     * gets the proportions of repetitions for each value
     *
     * @return
     */
   def getValuesProportions: List[Double] = {
      val concreteValues = getListValues
      val differentValues = getDifferentValues

      // gets the counter for each value
      val counters: List[Int] = differentValues.map(x => concreteValues.count(y => {
         y == x
      }))

      // computes and return the proportions for each value
      counters.map(x => {
         x / concreteValues.size.toDouble
      })
   }

   /**
     * Gets the size of the store
     *
     * @return number of indices represented, values stored, number of
     *         different values
     */
   def getSize: (Long, Long, Long)

   /**
     * Gets memory size for an object of variable type
     *
     * @return
     */
   def getMemorySize: Long

   /**
     * Produce the combination
     *
     * @param store target store to combine with
     * @return
     */
   def combine(store: ValueStore): ValueStore = {
      // perform combination
      combinationFunction(this, store)
   }

   /**
     * Marginalize operation
     *
     * @param variable target variable
     * @return
     */
   def marginalize(variable: Variable): ValueStore = {
      // perform the marginalization
      marginalizationFunction(this, variable)
   }

   /**
     * Normalization method
     *
     * @return
     */
   def normalize: ValueStore = {
      // several cases according to the number of variables
      val result = if (variables.getSize == 1) normalizeMarginalStore
      else normalizeConditionedStore

      // return result
      result
   }

   /**
     * Normalize values in stores representing marginal distributions
     *
     * @return
     */
   private def normalizeMarginalStore = {
      // gets the complete list of values
      val valuesInStore = getListValues
      val sum = Util.roundNumber(valuesInStore.sum)
      val modifiedValues: List[Double] = if (valuesInStore.sum == 1 || valuesInStore.sum == 0) {
         valuesInStore
      }
      else {
         valuesInStore.map(value => {
            Util.roundNumber(value / sum)
         })
      }

      // creates a new store with these new values
      ValueStore.createStore(this, modifiedValues)
   }

   /**
     * Normalize values for conditioned distributions
     *
     * @return
     */
   private def normalizeConditionedStore = {
      // gets all the values
      val vals = getListValues

      val groups: Iterator[List[Double]] = vals.sliding(mainVariable.getNumberStates)

      // maps groups into their sums
      val sums = groups.map(group => group.sum)

      // zip groups and sums
      val groupsAndSums: List[(List[Double], Double)] = groups.zip(sums).toList

      // process all the elements
      val normalizedValues: List[Double] = groupsAndSums.flatMap(group => {
         group._1.map(value => Util.roundNumber(value / group._2))
      })

      // create the store
      ValueStore.createStore(this, normalizedValues)
   }

   /**
     * toString method
     *
     * @return
     */
   def toString: String
}

/**
 * companion object of trait in order to store global methods
 * and configuration values
 */
object ValueStore extends Combiner{
   /**
    * type of combination to use
    */
   var combinationType : OperatorType = OperatorType.DEFAULT

   /**
    * type of marginalization to use
    */
   var marginalizationType : OperatorType = OperatorType.DEFAULT

   /**
    * Creates a store of the same type as this (one)
    *
    * @param values list of values for the store
    * @return
    */
   def createStore(store : ValueStore, values: List[Double]): ValueStore = {
      // creates the corresponding store
      /*val result = store match {
         case TableStore(vars, _) => TableStore(vars, values.toArray)
         case TreeStore(vars, _) => TreeStore(vars, values.toArray)
         case ArrayIMStore(vars, _ , _) => ArrayIMStore(vars, values.toArray)
         case ArrayIIStore(vars, _, _) => ArrayIIStore(vars, values.toArray)
         case MapSGStore(vars, _) => MapSGStore(vars, values.toArray)
         case MapLGStore(vars, _) => MapLGStore(vars, values.toArray)
         case MapLIIStore(vars, _) => MapLIIStore(vars, values.toArray)
         case MapLIMStore(vars, _) => MapLIMStore(vars, values.toArray)
         case MapSIMStore(vars, _) => MapSIMStore(vars, values.toArray)
         case ArraySIIStore(vars, _, _) => ArraySIIStore(vars, values.toArray)
         case ArraySIMStore(vars, _, _) => ArraySIMStore(vars, values.toArray)
         case MapIMStore(vars, _, _) => MapIMStore(vars, values.toArray)
      }*/
      val vars = store.variables
      val result = store.kind match {
         case ValueStoreTypes.TABLE => TableStore(vars, values.toArray)
         case ValueStoreTypes.TREE => TreeStore(vars, values.toArray)
         case ValueStoreTypes.PRUNEDTREE => PrunedTreeStore(vars, values.toArray)
         case ValueStoreTypes.IDPISTORE => IDPMStore(vars, values.toArray)
         case ValueStoreTypes.IDPMSTORE => IDPIStore(vars, values.toArray)
         case ValueStoreTypes.VDGSSTORE => VDGSStore(vars, values.toArray)
         case ValueStoreTypes.VDGLSTORE => VDGLStore(vars, values.toArray)
         case ValueStoreTypes.VDILISTORE => VDILIStore(vars, values.toArray)
         case ValueStoreTypes.VDILMSTORE => VDILMStore(vars, values.toArray)
         case ValueStoreTypes.VDISISTORE => VDISIStore(vars, values.toArray)
         case ValueStoreTypes.VDISMSTORE => VDISMStore(vars, values.toArray)
         case ValueStoreTypes.IDSISTORE => IDSIStore(vars, values.toArray)
         case ValueStoreTypes.IDSMSTORE => IDSMStore(vars, values.toArray)
         case ValueStoreTypes.IDMMSTORE => IDMMStore(vars, values.toArray)
      }

      // return result
      result
   }

   /**
    * auxiliary method for combining all the indexes leading
    * to a certain result
    * @param valst1 first potential
    * @param valst2 second potential
    * @param entry entry with result and triples with result,
    *              val in first potential and val in second
    *              potential
    * @return
    */
   def combineIndices(valst1 : ValueStore, valst2 : ValueStore,
                      entry : (Double, List[(Double, Double, Double)])) : (Double, ArrayBuffer[Long]) = {
      // creates the array of indexes for result
      val indexesForValue = ArrayBuffer[Long]()

      // considers each possible combination of values, only
      // when default value if not produced
      entry._2.foreach(alternative => {
         // gets the list of indexes for each pair of values
         indexesForValue ++= combineValues(valst1, valst2, alternative)
      })

      // return the final result
      (entry._1, indexesForValue)
   }

   /**
    * auxiliary method for combining a pair of values
    * @param valst1 first potential
    * @param valst2 second potential
    * @param triplet triplet with result, val1 and val2
    * @return  array of indexes for result
    */
   def combineValues(valst1 : ValueStore, valst2 : ValueStore,
                     triplet: (Double, Double, Double)): Array[Long] = {
      // creates a mapper object
      val mapper = CombineMapper(valst1.variables, valst2.variables)

      // get indexes for val1
      val indexes1 = valst1.getIndicesForValue(triplet._2)
      val indexes2 = valst2.getIndicesForValue(triplet._3)

      // make combinations of indexes in order to filter and keep
      // only those compatible
      val combinations = Util.makeCombinations(indexes1, indexes2)
      val compatible: List[(Long, Long)] = combinations.filter(pair => mapper.compatible(pair._1, pair._2))

      // now produces the projections of pairs into indexes in result
      if(compatible.isEmpty) Array[Long]()
      else compatible.map(pair => mapper.computeGlobalIndex(pair._1, pair._2)).toArray
   }
}
