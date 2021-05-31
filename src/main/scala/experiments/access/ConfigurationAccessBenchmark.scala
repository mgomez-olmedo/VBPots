package experiments.access

import base.Configuration
import experiments.generation.IndexesList
import org.scalameter.{Key, Warmer, config}
import potential.{Potential, ValueStoreTypes}

import scala.collection.mutable.ArrayBuffer

object ConfigurationAccessBenchmark extends App {

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 25,
      Key.verbose -> false
   ) withWarmer(new Warmer.Default)

   // parameters for the test: path to access
   val path = "./data/potentialObjects/1500-800-1000/"

   // defines the number of potentials to explore
   val numberPotentials : Int = 100


   // considers time accessing tables
   val cptTimes = ArrayBuffer.fill(numberPotentials)(0.0)

   // loop for each potential
   for(i <- 0 until numberPotentials) {
      // compose filename
      val filename = path + "pot-" + i + ".pot"
      // read the potential
      val potential = Potential.readObject(filename)

      // read the indexes file
      val indexFilename = path + "ind-" +i + ".ind"
      val indexes = IndexesList.readObject(indexFilename).indexes

      // compute access time
      val cptTime = standardConfig measure {
         // makes a configuration with the values of the store
         val configuration = Configuration(potential.variables)

         // loop over indexes, but making a prior conversion to
         // configuration
         for(j <- 0 until indexes.length){
            val values = Configuration.computeCoordinates(potential.variables, j)
            potential.store.getValue(configuration.setValues(values))
         }
      }

      cptTimes(i)=cptTime.value
   }

   println("Base access times: " + cptTimes.mkString(" "))
   println("Average: " + cptTimes.sum/cptTimes.length)

   // considers time accessing trees
   val treeTimes = ArrayBuffer.fill(numberPotentials)(0.0)

   // loop for each potential and Tree
   for(i <- 0 until numberPotentials) {
      // compose filename
      val filename = path + "pot-" + i + ".pot"
      // read the potential
      val potential = Potential.readObject(filename)

      // convert to tree
      val potTree = potential.convert(ValueStoreTypes.TREE)

      // read the indexes file
      val indexFilename = path + "ind-" +i + ".ind"
      val indexes = IndexesList.readObject(indexFilename).indexes

      // compute access time
      val treeTime = standardConfig measure {
         // makes a configuration with the values of the store
         val configuration = Configuration(potential.variables)

         // loop over indexes, but making a prior conversion to
         // configuration
         for(j <- 0 until indexes.length){
            val values = Configuration.computeCoordinates(potential.variables, j)
            potential.store.getValue(configuration.setValues(values))
         }
      }

      treeTimes(i)=treeTime.value
   }

   println("Tree access times: " + treeTimes.mkString(" "))
   println("Average: " + treeTimes.sum/treeTimes.length)

   // considers time accessing AIIM
   val aaimTimes = ArrayBuffer.fill(numberPotentials)(0.0)

   // loop for each potential and Tree
   for(i <- 0 until numberPotentials) {
      // compose filename
      val filename = path + "pot-" + i + ".pot"
      // read the potential
      val potential = Potential.readObject(filename)

      // convert to tree
      val aaim = potential.convert(ValueStoreTypes.IDSMSTORE)

      // read the indexes file
      val indexFilename = path + "ind-" +i + ".ind"
      val indexes = IndexesList.readObject(indexFilename).indexes

      // compute access time
      val aaimTime = standardConfig measure {
         // makes a configuration with the values of the store
         val configuration = Configuration(potential.variables)

         // loop over indexes, but making a prior conversion to
         // configuration
         for(j <- 0 until indexes.length){
            val values = Configuration.computeCoordinates(potential.variables, j)
            potential.store.getValue(configuration.setValues(values))
         }
      }

      aaimTimes(i)=aaimTime.value
   }

   println("AIM access times: " + aaimTimes.mkString(" "))
   println("Average: " + aaimTimes.sum/aaimTimes.length)

}
