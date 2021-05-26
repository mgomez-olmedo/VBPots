package potential

import base.{Variable, VariableSet}
import mapper.{CombineMapper, MarginalizeMapper}
import utils.{DataSizes, Util}

/**
  * Class for managing table storage (CPT) for potentials
  * @param variables list of variables
  */
case class TableStore(override val variables : VariableSet,
                      values:Array[Double]) extends ValueStore with
                     Combiner with Marginalizer {
   /**
    * defines the kind of store
    */
   override val kind: ValueStoreTypes.Value = ValueStoreTypes.TABLE

   /**
    * sets the behavior for default value treatment
    */
   //override val defaultValueComputerType: DefaultValueComputerType =
   //   DefaultValueComputerType.ZERO

   /**
     * Gets the value corresponding to a certain index
     *
     * @param index target index
     * @return corresponding value
     */
   override def getValue(index: Long): Double = {
      TableStore.addGetValueCalls
      values(index.toInt)
   }

   /**
     * Gets a complete list with the values stored in the table
     *
     * @return list of values
     */
   override def getListValues : List[Double] = {
      values.toList
   }

   /**
     * Gets the list of values without repeated values
     * @return list of values
     */
   override def getDifferentValues : List[Double] = {
      getListValues.distinct
   }

   /**
     * Gets the indices storing a certain value
     * @param value target value
     * @return list of indices
     */
   override def getIndicesForValue(value : Double) : List[Long] = {
      // consider all the values and filter indices
      values.indices.map(index => {
         if(values(index) == value) index.toLong
         else 0.toLong
      }).filter(index => index != 0).toList
   }

   /**
     * Gets the size of the store
     * @return tuple with number of represented indices, number of
     *         stored values and number of different values
     */
   override def getSize: (Long, Long, Long) = {
      // return a tuple with possible values, number of indices with
      // assigned values (equals to possible values), number of
      // values stored (equals to possible values) and number of
      // different values
      (values.length, values.length, values.distinct.length)
   }

   /**
     * toString method
     * @return string with object information
     */
   override def toString: String = {
      var output="TableValueStore.......................\n"
      output=output+variables.toString
      output=output+"Values: \n"
      output=output+values.mkString(" ")+"\n"
      val info = getSize
      output = output + " indices: " + info._1 + " values: " +
         info._2 + " diff. values: " + info._3 + "\n"
      output = output + "memory size: " + getMemorySize + "\n"
      output
   }

   /**
     * Gets memory size for an object of variable type
     * @return memory size estimation
     */
   def getMemorySize: Long = {
      // gets sizes due to domain
      val variableSize = variables.getMemorySize

      // gets sizes due to value
      val valuesSize = DataSizes.DOUBLE*values.length

      // considers the size of the data structure: array
      val storeSize = DataSizes.ARRAY

      // return the sum
      variableSize + valuesSize + storeSize
   }

   // register available functions for marginalization
   // and combination
   registerCombinationFunction(OperatorType.DEFAULT,
                              TableStore.combineDefault)
   registerCombinationFunction(OperatorType.ALT1,
                              TableStore.combineAlt1)
   registerMarginalizationFunction(OperatorType.DEFAULT,
                              TableStore.marginalizeDefault)
   registerMarginalizationFunction(OperatorType.ALT1,
                              TableStore.marginalizeAlt1)
}

/**
  * Companion object for TableStore
  */
object TableStore extends Combiner with Marginalizer{
   var getValueCalls = 0

   def addGetValueCalls = {
      getValueCalls+=1
   }

   def getGetValueCalls = getValueCalls

   /**
     * Apply method acting as factory method
     * @param variables variables for the potential
     * @param values values to assign
     * @return
     */
   def apply(variables : VariableSet, values : Array[Double]): TableStore = {
      val result=new TableStore(variables, values)

      // return result
      result
   }

   /**
     * Default method for performing combination
     * @param valst1 first potential to combine
     * @param valst2 second potential to combine
     * @return result of combination
     */
   override def combineDefault(valst1 : ValueStore, valst2 : ValueStore) : ValueStore = {
      val mapper = new CombineMapper(valst1.variables, valst2.variables)
      // Considers each index in finalPotential
      val values=(0L until mapper.resultDomainMaxIndex).
         map(index => {
            val mapped: (Long, Long) = mapper.mapIndices(index)
            Util.roundNumber(valst1.getValue(mapped._1)*valst2.getValue(mapped._2))
         }).toArray

      // Creates the final storage of values
      TableStore(mapper.resultDomain, values)
   }

   /**
     * Parallel method for performing combination
     * @param valst1 first potential to combine
     * @param valst2 second potential to combine
     * @return result of combination
     */
   def combineAlt1(valst1 : ValueStore, valst2 : ValueStore) :
                                                      ValueStore = {
      val mapper = new CombineMapper(valst1.variables, valst2.variables)
      // Considers each index in finalPotential
      val values=(0L until mapper.resultDomainMaxIndex).
         par.map(index => {
            val mapped: (Long, Long) = mapper.mapIndices(index)
            valst1.getValue(mapped._1)*valst2.getValue(mapped._2)
         }).toArray

      // Creates the final storage of values
      TableStore(mapper.resultDomain, values)
   }

   /**
     * Default method for marginalization
     * @param valst potential to marginalize
     * @param variable variable to remove
     * @return result of marginalization
     */
   override def marginalizeDefault(valst : ValueStore, variable : Variable) :
                                                      ValueStore = {
      val mapper = new MarginalizeMapper(variable, valst.variables)

      // Considers each index in the final potential
      val values=Range.Long(0, mapper.maxIndexResultDomain, 1).
         map(index => {
            // get the list of of indices related to index
            val indices: List[Long] = mapper.mapIndexFromResultToSource(index)
            val value = indices.map(
               indexInStore => valst.getValue(indexInStore)).sum
            Util.roundNumber(value)
         }).toArray

      // Creates the final storage of values
      TableStore(mapper.resultDomain, values)
   }

   /**
     * Default method for marginalization
     * @param valst potential to marginalize
     * @param variable variable to remove
     * @return result of marginalization
     */
   def marginalizeAlt1(valst : ValueStore, variable : Variable) :
                                                         ValueStore = {
      val mapper = new MarginalizeMapper(variable, valst.variables)

      // Considers each index in the final potential
      val values=Range.Long(0, mapper.maxIndexResultDomain, 1).
         par.map(index => {
            // get the list of of indices related to index
            val indices: List[Long] = mapper.mapIndexFromResultToSource(index)
            val value = indices.map(indexInStore => valst.getValue(indexInStore)).sum
            Util.roundNumber(value)
         }).toArray

      // Creates the final storage of values
      TableStore(mapper.resultDomain, values)
   }
}
