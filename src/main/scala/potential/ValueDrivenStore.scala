package potential

import utils.Util

/**
 * General trait for ValueDrivenStores, adding the behavior
 * of defining and using a default value
 */
trait ValueDrivenStore extends ValueStore {
   /**
    * adds a new value to the representation if the value
    * is not equals to the default value
    *
    * @param value value to add to the representation
    * @param index target index
    * @return
    */
   def addValue(value: Double, index: Long): ValueDrivenStore = {
      if (value == Util.DEFAULTVALUE) this
      else addValueForRepresentation(value, index)
   }

   /**
    * Adds a new value to the representation. The concrete
    * implementation depends on the kind of store
    *
    * @param value value to add
    * @param index target index
    * @return result of the operation
    */
   def addValueForRepresentation(value: Double, index: Long): ValueDrivenStore

   /**
    * Abstract method for generating a store for each value of the
    * store
    * @return
    */
   def split : List[ValueStore]

   /**
    * Abstract method for pruning
    * @param threshold maximum loss of entropy
    * @return
    */
   def prune(threshold : Double) : ValueDrivenStore

   /**
    * computes the different in information produced when making
    * a merge between two values
    * @param index
    * @param differentValues
    * @return
    */
   def computeDifference(index : Int, differentValues : List[Double]) : Double = {
      // get the values and the corresponding list of indexes
      val val1 = differentValues(index)
      val val2 = differentValues(index + 1)
      val sumSize1 = computeSumAndSizeForValue(val1)
      val sumSize2 = computeSumAndSizeForValue(val2)
      val sum = sumSize1._1 + sumSize2._1
      val size = sumSize1._2 + sumSize2._2

      // computes the difference and return it
      Math.log10(size/sum)*sum -
         Math.log(sumSize1._2/sumSize1._1)*sumSize1._1 -
         Math.log(sumSize2._2/sumSize2._1)*sumSize2._1
   }

   /**
    * computes the global sum for a given value
    * @param value
    * @return
    */
   def computeSumAndSizeForValue(value : Double) = {
      val indexes = getIndicesForValue(value)
      (value*indexes.size, indexes.size)
   }

   /**
    * merge two entries of the store producing a new one
    * @param value1
    * @param value2
    * @return
    */
   def merge(value1 : Double, value2 : Double) : ValueDrivenStore
}
