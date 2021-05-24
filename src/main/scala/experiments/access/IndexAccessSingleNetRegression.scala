package experiments.access

import bnet.Bnet
import org.scalameter._
import org.scalameter.api.SerializationPersistor
import org.scalameter.picklers.noPickler.instance
import potential.ValueStoreTypes

import scala.collection.mutable.HashMap
import scala.util.Random

object IndexAccessSingleNetRegression extends Bench.Regression {
   // defines the list of representations to consider
   val representations = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTIMMUT,
      ValueStoreTypes.IDPIMMUT,
      ValueStoreTypes.IDSETIMMUT,
      ValueStoreTypes.IDMMUT)

   // defines a data structure for storing the result for
   // each net and data structure
   val times: HashMap[String, HashMap[ValueStoreTypes.Value, Double]] = HashMap()
   val numberIndexes: HashMap[String, Long] = HashMap()

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 5,
      Key.exec.maxWarmupRuns -> 20,
      Key.exec.benchRuns -> 30,
      Key.verbose -> true
   ) withWarmer (new Warmer.Default)
   withMeasurer {
      new Measurer.IgnoringGC
   }

   // defines persistence of data
   override def persistor = new SerializationPersistor

   // defines the max number of configurations to consider for
   // the test
   val maxNumberOfConfigurations = 50000
   val minNumberOfConfigurations = 8000

   val netName = "child.net"
   val bnet = Bnet(netName)

   // gets the number of possible configurations considering
   // all the potentials
   val totalNumberConfigurations = bnet.potentials.map(_.variables.possibleValues).sum
   println("number of parameters: " + totalNumberConfigurations)

   // determine the number of configurations to access
   val numberConfigurations = if (totalNumberConfigurations < maxNumberOfConfigurations)
      Math.max(totalNumberConfigurations, minNumberOfConfigurations)
   else maxNumberOfConfigurations
   println("Number of configurations: " + numberConfigurations)

   // store the number of configurations used for testing
   numberIndexes += ((netName, numberConfigurations))

   val dimensions: Gen[Long] = Gen.single("numberConfigurations")(numberConfigurations)

   // for each configuration select a potential at random
   // and the corresponding index to access
   Random.setSeed(0L)

   val accessSet: Gen[List[(Int, Long)]] = for (
      dim <- dimensions
   ) yield {
      (0L until dim).map(index => {
         // gets the index of the potential to access
         val indexPotential = Random.nextInt(bnet.potentials.size)

         // and generates a random index as well
         val potentialSize = bnet.potentials(indexPotential.toInt).variables.possibleValues
         val indexInPotential = Math.abs(Random.nextLong()) % potentialSize

         // returns the tuple
         (indexPotential, indexInPotential)
      }).toList
   }

   println("starting with TABLE representation")
   // compute access time
   performance of "Table" in {
      measure method "getValue" in {
         using(accessSet) config(
            Key.exec.minWarmupRuns -> 5,
            Key.exec.maxWarmupRuns -> 20,
            Key.exec.benchRuns -> 30,
            Key.verbose -> true,
            Key.exec.independentSamples -> 1
         ) in {
            accessData => {
               (0 until accessData.size).foreach {
                  index => {
                     // gets the potential and index to access
                     val target: (Int, Long) = accessData(index)
                     val potential = bnet.potentials(target._1.toInt)
                     val value = potential.store.getValue(target._2)
                  }
               }
            }
         }
      }
   }

   // try the test with the rest of representations
   for(representation <- representations){
      val convertedNet = Bnet.convert(bnet, representation)

      // compute access time
      performance of representation.toString in {
         measure method "getValue" in {
            using(accessSet) config(
               Key.exec.minWarmupRuns -> 5,
               Key.exec.maxWarmupRuns -> 20,
               Key.exec.benchRuns -> 30,
               Key.verbose -> true,
               Key.exec.independentSamples -> 1
            ) in {
               accessData => {
                  (0 until accessData.size).foreach {
                     index => {
                        // gets the potential and index to access
                        val target: (Int, Long) = accessData(index)
                        val potential = bnet.potentials(target._1.toInt)
                        val value = potential.store.getValue(target._2)
                     }
                  }
               }
            }
         }
      }
   }
}
