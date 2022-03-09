package experiments.access

import bnet.Bnet
import potential.ValueStoreTypes

import java.lang.management.{ManagementFactory, ThreadMXBean}
import scala.collection.mutable.HashMap

/**
 * time object for measuring execution time
 */
package object SingleTimer {
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
    * remove outliers from data passed as argument
    * @param data list of data values to filter
    */
   def removeOutliers(data : List[Double]) : List[Double] = {
      // computes mean value
      val meanData = mean(data)

      // computes standard deviation
      val std = stdDeviation(data)
      println("mean : " + meanData+ " std: " + std)

      // filter values far from mean - std
      val result = data.filter(value => math.abs(value - meanData) <= std)

      println("removed data: " + (data.length - result.length))

      // return result
      result
   }

   def removeOutliers2(data : List[Double]) : List[Double] = {
      // computes mean value
      val meanData = mean(data)

      // filter values far from mean - std
      val result = data.sorted.take(4000)

      // return result
      result
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
   def measureCpuTime(f: => Unit, warmup : Int, iterations : Int): Double = {
      // perform warmup iterations
      (0 until warmup).foreach(iter => {
         val r = f
      })

      // compute times of execution
      val times = (0 until iterations).map(iter => {
         val start = getCpuTime
         val r = f
         val end = getCpuTime
         ((end - start)/1000000000.0)
      }).toList

      // return final measure of time
      val time = mean(removeOutliers(times))
      //val time = (end - start)

      // compute average and standard deviation
      time
   }
}

/**
 * Object for getting cpu time of a process
 */
object IndexAccessSingleNoScalaMeter extends App{
   // defines the list of representations to consider
   val representations = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLSTORE,
      ValueStoreTypes.VDILISTORE,
      ValueStoreTypes.IDPISTORE,
      ValueStoreTypes.IDSISTORE,
      ValueStoreTypes.IDMMSTORE)

   // defines a data structure for storing the result for
   // each net and data structure
   val times: HashMap[String, HashMap[ValueStoreTypes.Value, Double]] = HashMap()
   val numberIndexes: HashMap[String, Long] = HashMap()

   // defines the max number of configurations to consider for
   // the test
   val maxNumberOfConfigurations = 100000
   val minNumberOfConfigurations = 10000

   /**
    * evaluate a given representation in order to measure execution
    * time
    */
   def evaluateRepresentation(bnet : Bnet, representation : ValueStoreTypes.Value) : Unit = {
      println("executing evaluateRepresentation for net: " +
             bnet.name + " rep: " + representation.toString)
      //  convert the net
      val convertedNet = Bnet.convert(bnet, representation)

      // gets the measures of mean snd std
      val time = SingleTimer.measureCpuTime(evaluateExecutionTime(bnet),
         10000, 20000)
      println("Time before storing in map: " + time)

      // stores the info about the execution
      storeMeasure(bnet, representation, time)
   }

   /**
    * evaluate the execution time of a net and a given
    * access set
    * @param bnet net of interest
    * @param accessSet set of poetntials and indexes to access
    */
   def evaluateExecutionTime(bnet : Bnet) = {
      // now measure the time
      // makes all the call to getValue
      bnet.potentials.foreach(pot => {
         (0L until pot.store.getVariables.possibleValues).foreach(index => {
            pot.store.getValue(index)
         })
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
                    time : Double) = {
      // check if there is a map for the net
      val netInfo = times.get(bnet.name).getOrElse(null)

      // stores measure info
      if(netInfo == null){
         val newMap = HashMap[ValueStoreTypes.Value, Double]()

         // add info about the execution
         newMap += ((representation, time))

         // add the newMap to times dictionary
         times += ((bnet.name, newMap))
      }
      else{
         // just use netInfo
         netInfo += ((representation, time))
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
      val tableTime = dataForNet.get(ValueStoreTypes.TABLE).get
      println("Table time: " + tableTime)

      // for the rest of storages compute the savings
      val savings = representations.map(rep => {
         // gets the execution info
         val time = dataForNet.get(rep).get
         println(rep.toString + " time: " + time)

         // compute saving value
         "%11.9f".format((time*100.0/tableTime)-100)
      }).mkString(" & ")

      // compose and return the line
      netName + " & " + " & " + savings + "\\\\\\hline\n"
   }

   val netnames2 = List(
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

   val netnames = List("alarm.net")

   for(netname <- netnames){
      println("------------------------------------------")
      println("net: " + netname)
      val bnet = Bnet(netname)

      for (rep <- representations) {
         evaluateRepresentation(bnet, rep)
      }

      evaluateRepresentation(bnet, ValueStoreTypes.TABLE)

      println(computeRatios(bnet.name))
   }
}
