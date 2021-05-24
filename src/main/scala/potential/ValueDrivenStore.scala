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
}
