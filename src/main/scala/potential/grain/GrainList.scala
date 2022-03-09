package potential.grain

import utils.DataSizes

/**
 * Class for storing grains with a list
 *
 * @constructor creates an object with the information
 *              passed as argument
 * @param list list of grains stored in the object
 */
case class GrainList(list: List[Grain]) extends Iterable[Grain]{
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
    * adds a new grain to the list
    * @param grain
    * @return
    */
   def addGrain(grain : Grain) : GrainList = {
      val matches = grain::list.filter(grainPresent => grainPresent.isConsecutiveGrain(grain))

      // add the grain taking into account the matches
      if(!matches.isEmpty){
         // detect limits for the new grain to insert
         val minStart = matches.map(grain => grain.start).min
         val maxEnd = matches.map(grain => grain.end).max

         // remove grains contained in matches
         val newList = list.filter(keep => keep.end < minStart ||
                                    keep.start > maxEnd)

         // produce the new list
         new GrainList(Grain(minStart, maxEnd)::newList)
      }
      else{
         // just add the grain passed as argument
         new GrainList(grain :: list)
      }
   }

   /**
    * returns the list of grains
    *
    * @return
    */
   override def toList: List[Grain] = list

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
      val string2 = string1 + list.sorted.map(grain => grain.toString).mkString(" ")
      string2
   }

   /**
    * Makes class iterable
    * @return
    */
   override def iterator: Iterator[Grain] = list.iterator
}

/**
 * companion object of the class
 */
object GrainList{
   def merge(grains1 : GrainList, grains2 : GrainList) : GrainList = {
      var result = grains1
      grains2.foreach(grain => result = result.addGrain(grain))

      // return result
      result
   }
}
