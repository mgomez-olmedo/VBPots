package potential.grain

import utils.DataSizes

/**
 * Class for storing grains with a list
 *
 * @constructor creates an object with the information
 *              passed as argumentk
 * @param list list of grains stored in the object
 */
case class GrainList(list: List[Grain]) {
   /**
    * adds a new index to the list
    *
    * @param index index to assign to the grain
    * @return new object after including a new index
    * @note the objects of this class are immutable
    */
   def addIndex(index: Long): GrainList = {
      // look for a grain containing the index
      val grain = findIndex(index)

      // if no grain is found, just creates a new Grain for the
      // index and add it to the list
      if (grain == null) {
         GrainList(Grain(index) :: list)
      }
      else {
         // add the index to the grain detected
         val newGrain = grain.+(index).get
         val filtered: List[Grain] = list.filter(target => target != grain)
         GrainList(newGrain :: filtered)
      }
   }

   /**
    * returns the list of grains
    *
    * @return
    */
   def toList: List[Grain] = list

   /**
    * find an index in the list of grains
    *
    * @param index index to search for
    * @return grain containing the index or null
    */
   def findIndex(index: Long): Grain = {
      list.find(grain => grain.contains(index)).orNull
   }

   /**
    * gets a list will all the indices involved in the list
    *
    * @return list of indices involved in the object
    */
   def getIndices: List[Long] = list.
            flatMap(grain => grain.start to grain.end)

   /**
    * gets the number of indices stored in the list
    *
    * @return counter of indices stored in the object
    */
   def getSize: Long = list.map(grain => grain.getSize).sum

   /**
    * Gets memory size for an object of variable type
    *
    * @return memory size estimation
    */
   def getMemorySize: Int = {
      // gets sizes due to grains: considers the size of the
      // list itself (the same as for ARRAY)
      list.size * 2 * DataSizes.LONG //+ DataSizes.ARRAY
   }

   /**
    * Return the number of grains
    *
    * @return
    */
   def getNumberGrains: Int = list.size

   /**
    * return the max length of the grains in the list
    * @return
    */
   def getMaxLength: Long = {
      list.map(grain => grain.getSize).max
   }

   /**
    * return the average length of grains in the list
    * @return
    */
   def getAverageLength: Double = {
      list.map(grain => grain.getSize).sum/(list.size*1.0)
   }

   /**
    * toString method
    *
    * @return
    */
   override def toString: String = {
      val string1 = "GrainList( "
      val string2 = string1 + list.map(grain => grain.toString).mkString(" ") + ")\n"
      string2
   }
}
