package experiments.access

import bnet.Bnet
import org.scalameter._
import potential.ValueStoreTypes

import scala.util.Random

/**
 * Object for testing index access with different structures
 */
object IndexAccessBnetRepSelectBenchmark extends Bench.ForkedTime {

   // defines the list of representations to consider
   val representations = List(
      ValueStoreTypes.TABLE,
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTMUT,
      ValueStoreTypes.IDPMUT,
      ValueStoreTypes.IDMMUT)

   // defines the configuration for benchmarking
   val standardConfig = config(
      //Key.exec.minWarmupRuns -> 5,
      //Key.exec.maxWarmupRuns -> 20,
      Key.exec.benchRuns -> 20,
      //Key.exec.reinstantiation.frequency -> 2,
      //Key.exec.outliers.covMultiplier -> 1.5,
      //Key.exec.outliers.suspectPercent -> 40,
      Key.exec.independentSamples -> 1,
      Key.verbose -> false
   ) withWarmer {
      new Warmer.Default
   }
   withMeasurer {
      new Measurer.IgnoringGC
   }

   /**
    * prepares the access set for a given net
    * @param bnet
    * @param counter
    * @return
    */
   def prepareAccessSet(bnet: Bnet, counter : Long ) : List[(Int, Long)] = {
      // for each configuration select a potential at random
      // and the corresponding index to access
      Random.setSeed(0L)
      val accessSet: List[(Int, Long)] = (0L until counter).map(index => {
         // gets the index of the potential to access
         val indexPotential = Random.nextInt(bnet.potentials.size)

         // and generates a random index as well
         val potentialSize = bnet.potentials(indexPotential.toInt).variables.possibleValues
         val indexInPotential = Math.abs(Random.nextLong()) % potentialSize

         // returns the tuple
         (indexPotential, indexInPotential)
      }).toList

      // return the access set
      accessSet
   }

   /**
    * measures the execution time for a given bnet using an
    * access set passed as argument
    * @param bnet
    * @param accessSet
    * @return
    */
   def measureTime(bnet : Bnet, accessSet : List[(Int, Long)]) : Double = {
      (standardConfig measure {
         // perform the measure as many times as iterations shows
         for (i <- 0L until accessSet.length) {
            // gets the potential and index to access
            val target: (Int, Long) = accessSet(i.toInt)
            val potential = bnet.potentials(target._1.toInt)
            val value = potential.store.getValue(target._2)
         }
      }).value
   }

   /**
    * Analyzes the access to a certain network and a given
    * number of configurations
    */
   def analyzeNet(netName : String, extension : String,
                  representation : ValueStoreTypes.ValueStoreType,
                  numberConfigurations : Long) = {

      // compose file with serialized version and read the
      // corresponding object
      val filename = netName + "-obj-" + representation.toString +
           "." + extension

      // convert the bnet to the desired representation
      val bnet = Bnet.readObject(filename)

      // prepare access set
      val accessSet = prepareAccessSet(bnet, numberConfigurations)

      // compute access time
      var time = measureTime(bnet, accessSet)
      println(time)
   }

   // call batch method of analysis
   //batchAnalysis(folder, extension)
   //generatePaperLatexTable
   override def main(args : Array[String]) = {
      val extension = args(1)

      // retrieve the kind of representation to test
      val selectedRep = representations.filter(rep => rep.toString == args(2))

      // checks if this if a valid representation
      if(selectedRep.length == 0){
         println("Error in representation selection")
         System.exit(-1)
      }

      // keeps on processing
      var numberConfigurations : Long = 10000L
      analyzeNet(args(0), extension, selectedRep(0), numberConfigurations)
   }
   //println(composeLineForNet(args(0) + extension))
}
