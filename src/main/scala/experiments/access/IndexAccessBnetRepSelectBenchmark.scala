package experiments.access

import bnet.Bnet
import experiments.serializeNets.NetSerializator
import org.scalameter._
import potential.ValueStoreTypes
import potential.indexBased.IDPMStore

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
      ValueStoreTypes.VDGLSTORE,
      ValueStoreTypes.VDILMSTORE,
      ValueStoreTypes.IDPMSTORE,
      ValueStoreTypes.IDMMSTORE)

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 10,
      Key.exec.maxWarmupRuns -> 50,
      Key.exec.benchRuns -> 50,
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
      // val bnet = Bnet.readObject(filename)
      val bnet = NetSerializator.readSerializedNet(netName + "." + extension, representation)

      // prepare access set
      val accessSet = AccessSetUtils.prepareAccessSet(bnet, numberConfigurations)

      // compute access time
      var time = measureTime(bnet, accessSet)
      println("\n" + time)
   }

   // call batch method of analysis
   //batchAnalysis(folder, extension)
   //generatePaperLatexTable
   override def main(args : Array[String]) = {
      //val extension = args(1)
      val extension = "net"
      val netName = "alarm"

      // retrieve the kind of representation to test
      // val selectedRep = representations.filter(rep => rep.toString == args(2))
      val selectedRep = ValueStoreTypes.IDPMSTORE

      // checks if this if a valid representation
      //if(selectedRep.length == 0){
      //   println("Error in representation selection")
      //   System.exit(-1)
      //}

      // keeps on processing
      var numberConfigurations : Long = 20000L
      //analyzeNet(args(0), extension, selectedRep(0), numberConfigurations)
      analyzeNet(netName, extension, selectedRep, numberConfigurations)

      //println("Calls: " + TableStore.getGetValueCalls)
      println("Calls: " + IDPMStore.getGetValueCalls)
   }
   //println(composeLineForNet(args(0) + extension))
}
