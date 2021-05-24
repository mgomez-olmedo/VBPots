package experiments.access

import bnet.Bnet
import potential.ValueStoreTypes

import java.lang.management.{ManagementFactory, ThreadMXBean}
import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * private class for storing info about a execution
 */
class ExecutionInfo(val mean : Double, val std : Double,
                            val max : Double, val min : Double)

/**
 * time object for measuring execution time
 */
package object Timer {
   // data member for getting execution times
   val bean: ThreadMXBean = ManagementFactory.getThreadMXBean()

   /**
    * computes the mean of a sequence of values
    */
   def mean(times: Seq[Double]): Double =
               (times.sum) / times.size

   /**
    * computes the standard deviation of a sequence
    * of values
    * @param
    */
   def stdDeviation(times: Seq[Double]): Double = {
      val avg = mean(times)
      math.sqrt(times.map(x => math.pow((x - avg),2)).sum / times.size)
   }

   /**
    * gets the cpu time
    */
   def getCpuTime = if (bean.isCurrentThreadCpuTimeSupported())
      bean.getCurrentThreadCpuTime()
      else 0

   /**
    * Runs the argument function f and measures the user+system time
    * spent in it in seconds.
    * Accuracy depends on the system, preferably not used for runs
    * taking less than 0.1 seconds.
    * Returns a pair consisting of
    * - the return value of the function call and
    * - the time spent in executing the function.
    */
   def measureCpuTime(f: => Unit, iter : Int): ExecutionInfo = {
      // compute times of execution
      val times: List[Double] = (0 until iter).map(index => {
         val start = getCpuTime
         val r = f
         val end = getCpuTime

         // return final measure of time
         (end - start) / 1000000000.0
      }).toList

      println("times: " + times.mkString(" "))
      print(" avg: " + mean(times))
      print(" std: " + stdDeviation(times))
      print(" max: " + times.max)
      print(" min: " + times.min)
      println("\n-----------------------------")

      // compute average and standard deviation
      new ExecutionInfo(mean(times), stdDeviation(times),
                        times.max, times.min)
   }
}

/**
 * Object for getting cpu time of a process without using
 * scalameter
 */
object IndexAccessNoScalameter extends App{
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
   val times: HashMap[String, HashMap[ValueStoreTypes.Value, ExecutionInfo]] = HashMap()
   val numberIndexes: HashMap[String, Long] = HashMap()

   // defines the max number of configurations to consider for
   // the test
   val maxNumberOfConfigurations = 100000
   val minNumberOfConfigurations = 10000

   /**
    * prepares the access set for a given network
    * @param bnet
    */
   def prepareAccessSet(bnet : Bnet) = {
      // gets the number of possible configurations considering
      // all the potentials
      val totalNumberConfigurations =
      bnet.potentials.map(_.variables.possibleValues).sum
      println("number of parameters: " + totalNumberConfigurations)

      // determine the number of configurations to access
      val numberConfigurations =
         if (totalNumberConfigurations < maxNumberOfConfigurations)
            Math.max(totalNumberConfigurations, minNumberOfConfigurations)
         else maxNumberOfConfigurations
      println("Number of configurations: " + numberConfigurations)

      // store the number of configurations used for testing
      numberIndexes += ((bnet.name, numberConfigurations))

      // for each configuration select a potential at random
      // and the corresponding index to access
      Random.setSeed(0L)

      // compose the access set
      (0L until numberConfigurations).map(index => {
         // gets the index of the potential to access
         val indexPotential = Random.nextInt(bnet.potentials.size)

         // and generates a random index as well
         val potentialSize =
               bnet.potentials(indexPotential.toInt).variables.possibleValues
         val indexInPotential = Math.abs(Random.nextLong()) % potentialSize

         // returns the tuple
         (indexPotential, indexInPotential)
      }).toList
   }

   /**
    * evaluate a given representation in order to measure execution
    * time
    */
   def evaluateRepresentation(bnet : Bnet, representation : ValueStoreTypes.Value,
               repetitions : Int, accessSet : List[(Int, Long)]) : Unit = {
      println("executing evaluateRepresentation for net: " +
             bnet.name + " rep: " + representation.toString)
      //  convert the net
      val convertedNet = Bnet.convert(bnet, representation)

      // gets the measures of mean snd std
      val measures = Timer.measureCpuTime(evaluateExecutionTime(bnet,
                                    accessSet), repetitions)

      // stores the info about the execution
      storeMeasure(bnet, representation, measures)
   }

   /**
    * evaluate the execution time of a net and a given
    * access set
    * @param bnet net of interest
    * @param accessSet set of poetntials and indexes to access
    */
   def evaluateExecutionTime(bnet : Bnet, accessSet : List[(Int, Long)]) = {
      // now measure the time
      // makes all the call to getValue
      (0 until accessSet.size).foreach(index => {
         val target = accessSet(index)
         bnet.potentials(target._1).store.getValue(target._2)
      })
   }

   /**
    * store the measure of a execution time for a net
    * and a representation
    * @param bnet target net
    * @param representation target representation
    * @param measure measure to store
    */
   def storeMeasure(bnet : Bnet, representation : ValueStoreTypes.Value,
                    measure : ExecutionInfo) = {
      // check if there is a map for the net
      val netInfo = times.get(bnet.name).getOrElse(null)

      // stores measure info
      if(netInfo == null){
         val newMap = HashMap[ValueStoreTypes.Value, ExecutionInfo]()

         // add info about the execution
         newMap += ((representation, measure))

         // add the newMap to times dictionary
         times += ((bnet.name, newMap))
      }
      else{
         // just use netInfo
         netInfo += ((representation, measure))
      }
   }

   /**
    * compute the ratios for a given net
    * @param netName name of target net
    */
   def computeRatios(netName : String) : String = {
      // gets the measurements for the net (map with keys
      // representations and memory sizes
      val dataForNet = times.get(netName).get

      // gets the base size: for TABLE
      val tableExecInfo = dataForNet.get(ValueStoreTypes.TABLE).get

      // gets the number of tested indexes
      val indexes = numberIndexes.get(netName).get

      // for the rest of storages compute the savings
      val savings = representations.map(rep => {
         // gets the execution info
         val execInfo = dataForNet.get(rep).get

         // compute saving value
         "%07.2f".format((execInfo.mean*100.0/tableExecInfo.mean)-100)
      }).mkString(" & ")

      // compose and return the line
      netName + " & " + indexes + " & " + savings + "\\\\\\hline\n"
   }

   val netnames = List(
      "cancer.net",
      "asia.net",
      "survey.net",
      "sachs.net",
      "child.net",
      "alarm.net",
      "win95pts.net",
      "insurance.net",
      "hepar2.net",
      "andes.net",
      "hailfinder.net",
      "pigs.net",
      "water.net",
      "munin1.net",
      "link.net",
      "munin2.net",
      "munin3.net",
      "pathfinder.net",
      "munin4.net",
      "munin.net",
      "barley.net",
      "diabetes.net",
      "mildew.net"
   )

   val repetitions = 30

   for(netname <- netnames){
      println("------------------------------------------")
      println("net: " + netname)
      val bnet = Bnet(netname)
      val accessSet = prepareAccessSet(bnet)
      evaluateRepresentation(bnet, ValueStoreTypes.TABLE,
         repetitions, accessSet)

      for (rep <- representations) {
         evaluateRepresentation(bnet, rep, repetitions, accessSet)
      }

      println(computeRatios(bnet.name))
   }
}
