package potential.grain

import utils.DataSizes

/**
 * Class for supporting grains. A grain is a sequence of
 * consecutive indices storing the same probability (or
 * utility) value
 * @constructor generates an object using the information
 *              described as arguments
 * @param start initial index
 * @param end final index
 */
case class Grain(start: Long, end: Long) extends Comparable[Grain]
                                          with Serializable {
   /**
    * Determines if an index belong to the grain
    *
    * @param index target index
    * @return result of check
    */
   def contains(index: Long): Boolean = index >= start && index <= end

   /**
    * Checks if a given index is consecutive to the inidices contained
    * in the grain
    *
    * @param index target index
    * @return result of check
    */
   def isConsecutiveIndex(index: Long): Boolean =
      (index == start - 1) || (index == end + 1)

   /**
    * Checks if two grains are consecutive
    *
    * @param grain target garin
    * @return result of check
    */
   def isConsecutiveGrain(grain: Grain): Boolean = {
      (grain.start == (end + 1)) || ((grain.end + 1) == start)
   }

   /**
    * Produces a new grain after adding a new index
    *
    * @param index target index
    * @return grain after including a new index.
    * @note the objects of this class are immutable
    */
   def +(index: Long): Option[Grain] = {
      val minus = start - 1
      val plus = end + 1
      index match {
         case `minus` => Option(new Grain(minus, end))
         case `plus` => Option(new Grain(index, plus))
         case _ => Option(this)
      }
   }

   /**
    * Method to compare two grains
    *
    * @param other object to compare with
    * @return result of check
    */
   override def compareTo(other: Grain): Int = {
      if (this.start < other.start) -1
      else {
         if (this.start <= other.start && this.end >= other.end) 0
         else 1
      }
   }

   /**
    * Gets the size of the grain
    *
    * @return number of indices represented byt the grain
    */
   def getSize: Long = end - start + 1

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getObjectSize: Long = {
      2 * DataSizes.LONG
   }

   /**
    * toString method: composes a string with the information
    * about a Grain object
    *
    * @return
    */
   override def toString: String = {
      "[" + start + " - " + end + "] "
   }
}

/**
 * Companion object
 */
object Grain {

   /**
    * Method for making new objects
    *
    * @param indices indices of grain
    * @return
    */
   def apply(indices: Array[Long]): Grain = {
      new Grain(indices.head, indices.last)
   }

   /**
    * Constructor for a single index grain
    *
    * @param index index to assign to the grain
    * @return
    */
   def apply(index: Long): Grain = {
      new Grain(index, index)
   }

   /**
    * creates a new grain as a merge of two grains: both of
    * them must be consecutive
    * @param grain1
    * @param grain2
    * @return
    */
   def merge(grain1 : Grain, grain2 : Grain) : Grain = {
         if(grain1.start < grain2.start){
            new Grain(grain1.start, grain2.end)
         }
         else{
            new Grain(grain2.start, grain1.end)
         }
   }
}
