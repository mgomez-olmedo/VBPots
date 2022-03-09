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
   def prune(threshold : Double) : ValueDrivenStore = {

      // makes a map for storing the information loss for each
      // possible merge
      val loosesStore: collection.mutable.Map[(Double, Double), Double] =
      collection.mutable.Map()

      /**
       * auxiliar method for recursive merge of values
       *
       * @param accumulatedLoss
       * @param globalLoss
       * @param store
       * @return
       */
      def go(accumulatedLoss: Double, globalLoss: Double,
             store: ValueDrivenStore,
             loosesStore: collection.mutable.Map[(Double, Double), Double]): ValueDrivenStore = {

         // get different values
         val differentValues = store.getDifferentValues

         // acts only if there are several values
         if (differentValues.size > 1) {
            // update loss information if required
            (0 until differentValues.size - 1).foreach(index => {
               // gets succesive values
               val value1 = differentValues(index)
               val value2 = differentValues(index + 1)

               // update only for new pairs of values
               if (!loosesStore.contains((value1, value2))) {
                  val loss = store.computeInformationLoss(value1, value2)

                  // add the new information to the store
                  loosesStore += ((value1, value2) -> loss)
               }
            })

            // gets entry with min value of loss
            val minEntry = loosesStore.minBy(_._2)

            // checks if the merge will be performed
            if (accumulatedLoss + minEntry._2 < globalLoss) {
               // make new merge
               val result = store.merge(minEntry._1._1, minEntry._1._2)

               // removes the entry producing the merge
               val newLoosesStore = loosesStore.filter(
                  entry => entry._1._1 != minEntry._1._1 &&
                     entry._1._2 != minEntry._1._2 &&
                     entry._1._2 != minEntry._1._1 &&
                     entry._1._1 != minEntry._1._2)

               // makes a new call for testing new operations
               go(accumulatedLoss + minEntry._2, globalLoss, result, newLoosesStore)
            }
            else {
               // just return store
               store
            }
         }
         else {
            store
         }
      }
      // just call the recursive auxiliar method
      go(0, threshold, this, loosesStore)
   }

   /**
    * computes the different in information produced when making
    * a merge between two values
    * @param index
    * @param differentValues
    * @return
    */
   def computeInformationLoss(value1 : Double, value2 : Double) : Double = {
      // get the values and the corresponding list of indexes
      val sumSize1 = computeSumAndSizeForValue(value1)
      val sumSize2 = computeSumAndSizeForValue(value2)
      val sum = sumSize1._1 + sumSize2._1
      val size = sumSize1._2 + sumSize2._2

      // computes the difference and return it
      Math.log10(size/sum)*sum -
         Math.log10(sumSize1._2/sumSize1._1)*sumSize1._1 -
         Math.log10(sumSize2._2/sumSize2._1)*sumSize2._1
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
