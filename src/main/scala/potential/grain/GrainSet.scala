package potential.grain

import utils.DataSizes

import scala.collection.immutable.TreeSet

/**
 * Class for storing a collection of grains using a TreeSet
 *
 * @constructor creates a new object using the arguments
 * @param set TreeSet of grains with the content of object
 */
case class GrainSet(set: TreeSet[Grain]) {
   /**
    * adds a new index to the list
    *
    * @param index index to add
    * @return result of addition
    * @note the objects of this class are immutable
    */
   def addIndex(index: Long): GrainSet = {
      val grain = findIndex(index)

      // if no grain is found, then creates a new one
      if (grain == null) {
         GrainSet(set + Grain(index))
      }
      else {
         // just add a new index to the grain
         val newGrain = grain.+(index).get
         val filtered: TreeSet[Grain] =
                  set.filter(target => target != grain)
         GrainSet(filtered + newGrain)
      }
   }

   /**
    * returns the set of grains
    *
    * @return set of grains
    */
   def toSet: TreeSet[Grain] = set

   /**
    * find an index in the list of grains
    *
    * @param index target index
    * @return grain containing the index or null
    */
   def findIndex(index: Long): Grain = {
      set.find(grain => grain.contains(index)).orNull
   }

   /**
    * gets a list will all the indices involved in the set
    *
    * @return list of indices stored in the object
    */
   def getIndices: List[Long] = set.toList.flatMap(grain => grain.start to grain.end)

   /**
    * gets the number of indices stored in the set
    *
    * @return indices counter
    */
   def getNumberOfIndices: Long =
      set.toList.map(grain => {
         grain.getSize
      }).sum

   /**
    * Return the number of grains
    *
    * @return number of grains
    */
   def getNumberGrains: Int = set.size

   /**
    * gets the number of indices stored in the set
    *
    * @return  number of indices stored in the set
    */
   def getSize: Long = set.map(grain => grain.getSize).sum

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Long = {
      // considers the pairs of long values for each grain and
      // same of the set itself
      getNumberGrains * 2 * DataSizes.LONG //+ DataSizes.SET
   }

   /**
    * toString method
    *
    * @return string with info of object content
    */
   override def toString: String = {
      val string1 = "GrainSet( "
      val string2 = string1 + set.map(grain => grain.toString).
                  mkString(" ") + ")\n"
      string2
   }
}
