package experiments.statistics

import base.{Variable, VariableSet}
import potential.Potential
import potential.ValueStoreTypes.ValueStoreType

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}

/**
 * Class for storing statistics information about the
 * execution of an inference algorithm with graphical
 * models
 */
class ExecutionStatistics(val baseType: ValueStoreType) {
   /**
    * stores the sizes for removing a variable and
    * using a certain representation
    */
   val potSizes =
      Map[Variable, Map[ValueStoreType, ArrayBuffer[Long]]]()

   /**
    * stores the available potentials at each step
    * and their sizes according to several representation
    * types
    */
   val availablePotentials =
      Map[Potential, Map[ValueStoreType, Long]]()

   /**
    * stores potential sizes for reducing computation time
    */
   val cacheSizes = Map[VariableSet, Map[ValueStoreType, Long]]()

   /**
    * stores measures of memory comsumption
    */
   var maxSystemMemory = 0.0
   var avgSystemMemory = 0.0
   var samples: Long = 0

   /**
    * thread in charge of getting memory measurements
    */
   val fetcher: Fetcher = new Fetcher(this)

   /**
    * vars for controlling cache work
    */
   var hits = 0
   var checks = 0

   // starts fetcher work
   fetcher.start

   /**
    * Reset information of available potentials
    *
    * @param potentials potentials of interest
    */
   def resetAvailablePotentials(potentials: List[Potential],
                                storeType: ValueStoreType) = {
      // reset map information
      availablePotentials.clear()

      // stores potential information
      potentials.foreach(potential => {
         availablePotentials.put(potential, Map[ValueStoreType, Long]())
         val baseMap = availablePotentials.get(potential).get
         baseMap.put(storeType, potential.getMemorySize)
      })
   }

   /**
    * Removes a potential from available potentials
    *
    * @param potential potential of interest
    */
   def removeAvailablePotential(potential: Potential): Unit = {
      availablePotentials.remove(potential)
   }

   /**
    * Adds a new potential to availablePotential
    *
    * @param potential potential of interest
    */
   def addAvailablePotential(potential: Potential): Unit = {
      val mapForPotential = Map[ValueStoreType, Long]()
      mapForPotential.put(baseType, potential.getMemorySize)
      availablePotentials.put(potential, mapForPotential)
   }

   /**
    * Adds info for a potential under a certain
    * representation
    *
    * @param potential potential of interest
    * @param storeType type of storage
    * @param size size of potential
    */
   def addPotentialInfo(potential: Potential,
                        storeType: ValueStoreType,
                        size: Long): Unit = {
      // get the info about the potential
      val potInfo: mutable.Map[ValueStoreType, Long] =
               availablePotentials.get(potential).get

      // now store the info about the representation
      potInfo.put(storeType, size)
   }

   /**
    * Adds the measure for a potential included into
    * the list of available potential
    *
    * @param potential potential of interest
    * @param storeType type of storage
    */
   def addMeasureForRepresentation(potential: Potential,
                                   storeType: ValueStoreType): Unit = {
      val sizesForPot = availablePotentials.get(potential).get

      // check if the potential is contained into
      // cacheSizes if this is not the case, make
      // the corresponding conversion for storing
      // the new value
      val cacheInfo: Long = getCacheInfo(potential, storeType)

      // stores the annotation
      sizesForPot.put(storeType, cacheInfo)
   }

   /**
    * stores a new memory size of available potentials
    * for a variable and store type
    *
    * @param variable variable of interest
    * @param storeType type of storage
    */
   def storeSizeMeasure(variable: Variable,
                        storeType: ValueStoreType): Unit = {
      // gets the buffer for variable and store type
      // and creates if required
      var mapForVar = potSizes.get(variable).
               getOrElse(Map[ValueStoreType, ArrayBuffer[Long]]())

      // if the map is just created for the access,
      // then add it to potSizes
      if (mapForVar.size == 0) {
         // add the entry to potSizes
         potSizes.put(variable, mapForVar)
      }

      // gets the buffer for representation or creates
      // if required
      val buffer: ArrayBuffer[Long] = mapForVar.getOrElse(storeType,
                                          ArrayBuffer[Long]())

      // if the buffer is just created, add it to
      // mapForVar
      if (buffer.size == 0) {
         mapForVar.put(storeType, buffer)
      }

      // gets all the measures for all the potentials
      val measures: List[mutable.Map[ValueStoreType, Long]] =
         availablePotentials.map(entry => entry._2).toList

      // gets all the measures for the required type
      val measuresForType: List[Long] =
         measures.map(potMap => potMap.get(storeType).get)

      // now stores the sum
      buffer += measuresForType.sum
   }

   /**
    * get max potsizes for each representation
    *
    * @return dictionary with info with entries of type
    *         (type of storage - max potencial size)
    */
   def getMaxPotSizes = {
      // first at all gets a map having variable as key
      // and associated value a map with type and long.
      // The long value represents the max value obtained
      // for each representation
      val maxForVarsRep:
         Predef.Map[Variable, Predef.Map[ValueStoreType, Long]] =
         potSizes.keys.map(variable => {
            val maxReps =
               potSizes.get(variable).get.keys.map(rep => {
            val max =
               potSizes.get(variable).get.get(rep).get.max
            (rep, max)
         }).toMap
         (variable, maxReps)
      }).toMap

      // now it is needed to get the max for a given
      // representation examining all the variables
      val reps: List[ValueStoreType] =
         potSizes.get(potSizes.keys.head).get.keys.toList
      val vars: List[Variable] = potSizes.keys.toList

      // for each representation gets the max value
      val maxs: Predef.Map[ValueStoreType, Long] =
         reps.map(rep => {
            val max = vars.map(variable => {
               maxForVarsRep.get(variable).get.get(rep).get
            }).toList.max
            (rep, max)
         }).toMap

      // return maxs collection
      maxs
   }

   /**
    * gets a new memory sample
    */
   def getMemorySample: Unit = {
      System.gc
      val runtime = Runtime.getRuntime
      val sample = runtime.maxMemory() - runtime.freeMemory()
      if (sample > maxSystemMemory) {
         maxSystemMemory = sample
      }

      // computes new average value
      avgSystemMemory =
         (avgSystemMemory * samples + sample) / (samples + 1)
      samples += 1
   }

   /**
    * shows potential sizes
    */
   def showPotentialSizes = {
      // gets and show the keys of potSizes
      val variables: List[Variable] =
                     potSizes.keySet.toList

      // considers each representation
      val representations: List[ValueStoreType] =
         potSizes.get(variables.head).get.keySet.toList

      representations.foreach(representation => {
         // shows the representation
         println(representation)

         // considers each variable and the corresponding
         // sizes
         variables.foreach(variable => {
            // print variable
            print(variable.name + " -> ")

            // print sizes for the corresponding
            // representation
            val measures: ArrayBuffer[Long] =
               potSizes.get(variable).get.get(representation).get
            println(measures.mkString(" "))
         })
      })
      println()
   }

   /**
    * Gets info for a given potential in cache sizes.
    * If required store a new map for the potential
    *
    * @param potential potential of interest
    * @param storeType type of storage
    * @return
    */
   def getCacheInfo(potential: Potential,
                    storeType: ValueStoreType): Long = {
      // increments the number of checks
      checks = checks + 1

      println("getting cache info for: " +
         potential.variables.simpleToString + " -> " + storeType)
      println("hits: " + hits + " checks: " + checks +
            " size: " + cacheSizes.size)
      val key = potential.variables
      val keyInfo = cacheSizes.contains(key)

      val size = if (keyInfo) {
         // there is info for such potential
         // check now if contains info for the given type
         val storeInfo = cacheSizes.get(key).contains(storeType)
         if (storeInfo) {
            hits = hits + 1
            cacheSizes.get(key).get.get(storeType).get
         }
         else {
            // required conversion
            val newSize = potential.convert(storeType).getMemorySize

            // store this info
            val infoForPot = cacheSizes.get(key).get
            infoForPot.put(storeType, newSize)

            // return new size
            newSize
         }
      }
      else {
         // no info about this potential. A new dictionary
         // is required
         val entry = Map[ValueStoreType, Long]()

         // conversion is required as well
         val newSize = potential.convert(storeType).getMemorySize

         // add size for the given type
         entry.put(storeType, newSize)
         cacheSizes.put(key, entry)

         // finally return newSize
         newSize
      }

      // return size
      size
   }

   /**
    * gets the max value of system memory
    *
    * @return max value of system memory
    */
   def getMaxMemSize: Double = maxSystemMemory

   /**
    * gets the average value of system memory
    *
    * @return avg size of system memory
    */
   def getAvgMemSize: Double = avgSystemMemory

   /**
    * gets the number of samples
    *
    * @return number of samples
    */
   def getNumberSamples = samples

   /**
    * stops fetcher activity
    */
   def stopFetcher = {
      fetcher.finish
   }

   /**
    * shows info about the progress of the algorithm execution
    *
    * @return string with object information
    */
   override def toString: String = {
      val vars = potSizes.keys.toList

      var output =
         "---------------- ExecutionStatisticsInfo --------------------\n"
      output = output + "Info about potential sizes: \n"
      output = output + showVarsInfo + "\n"
      output = output + "-----------------------------------------\n"
      output = output + getMaxPotSizes.mkString("\n") + "\n"
      output = output + getAvgMemSize + "\n"
      output
   }

   /**
    * auxiliar method for showing information about
    * the vars
    * @return string with the desired information
    */
   def showVarsInfo: String = {
      val vars = potSizes.keys.toList
      vars.map(variable => {
         variable.name + " -> \n" + showVarRepInfo(variable)
      }).mkString("\n")
   }

   /**
    * auxiliar method dor showing information about
    * the values for a given variable
    * @param variable target variable
    * @return string with information about the object
    */
   def showVarRepInfo(variable: Variable): String = {
      val reps = potSizes.get(variable).get.keys.toList

      // consider each rep
      reps.map(rep => {
         rep.toString() + ": " +
            potSizes.get(variable).get.get(rep).get.mkString(", ")
      }).mkString("\n")
   }
}

/**
 * Class for performing memory measures from time
 * to time
 *
 * @param stats
 */
class Fetcher(val stats: ExecutionStatistics) extends Thread {
   /**
    * flag to show if the thread must be stoped
    */
   var terminated = false

   /**
    * run method for the thread
    */
   override def run(): Unit = {
      // call garbage collector before beginning
      // samples adquisition
      System.gc
      while (!terminated) {
         // gets the measurement of memory
         stats.getMemorySample
         Thread.sleep(1)
      }
      // new call to garbabe colelctor
      System.gc
   }

   // stops thread execution
   def finish = terminated = true
}
