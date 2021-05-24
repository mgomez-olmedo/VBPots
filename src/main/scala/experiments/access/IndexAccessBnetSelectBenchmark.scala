package experiments.access

import bnet.Bnet
import org.scalameter._
import potential.ValueStoreTypes

import java.io.{File, PrintWriter}
import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * Object for testing index access with different structures
 */
object IndexAccessBnetSelectBenchmark extends Bench.ForkedTime {

   // defines the list of representations to consider
   val representations = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTMUT,
      ValueStoreTypes.IDPMUT,
      ValueStoreTypes.IDMMUT)

   // defines a data structure for storing the result for
   // each net and data structure
   val times :  HashMap[String, HashMap[ValueStoreTypes.Value, Double]] = HashMap()
   val numberIndexes : HashMap[String, Long] = HashMap()

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
    * makes the analysis for a net
    * @param netname
    * @param extension
    * @param folder
    * @param numberConfigurations
    */
   def singleAnalysis(netname : String, extension : String,
                      numberConfigurations : Long) = {
      println("starting analysis of net " + netname)
      analyzeNet(netname, extension, numberConfigurations)
      println("ended analysis of net")
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
                  numberConfigurations : Long) = {
      // read bnet file and makes Bnet object
      println("staring analysis of net: " + netName)
      val bnet = Bnet(netName + "." + extension)

      // store the number of configurations used for testing
      numberIndexes += ((netName, numberConfigurations))

      // prepare access set
      val accessSet = prepareAccessSet(bnet, numberConfigurations)

      println("starting with TABLE representation")
      // compute access time
      var time = measureTime(bnet, accessSet)
      println("TABLE time: " + time)

      // creates the store for the net name
      val timesNet = HashMap[ValueStoreTypes.Value, Double]()

      // stores the data in the map
      timesNet += ((ValueStoreTypes.TABLE, time))

      // measure the time for the rest of representations
      representations.foreach(representation => {
         println("reading serialized object " + representation.toString)
         val filename = netName + "-obj-" + representation.toString +
            "." + extension

         // convert the bnet to the desired representation
         val convertedNet = Bnet.readObject(filename)

         println("starting with " + representation.toString)
         // try the list of access
         time = measureTime(convertedNet, accessSet)
         println(representation.toString + " time: " + time)

         // store into the map
         timesNet += ((representation, time))
      })

      // print summary information
      val timeTable: Double = timesNet.get(ValueStoreTypes.TABLE).get

      // show percentages of savings or lost
      representations.foreach(representation => {
         val timerep = timesNet.get(representation).get
         val saving = (timerep * 100.0 / timeTable) - 100
         println("perc for " + representation.toString + " = " + saving +
            " timerep: " + timerep + " timeTable: " + timeTable)
      })

      // store timesNet into times
      times += ((netName, timesNet))

      // shows information
      println(composeLineForNet(netName))
   }

   /**
    * generate the latex table as it appears in the paper
    * @param folder
    * @param extension
    */
   def generatePaperLatexTable  = {
      // creates the file with the data
      val texFile =
         new PrintWriter(new File("./analysis/savingsIndexAccess.tex"))

      // compose the header of the table
      val header=composeHeader()

      // composes the lines for each net
      val netLines = times.keys.map(
         net => composeLineForNet(net)).mkString

      // compose the final part of table declaration
      val finalLines = composeTableFinal()

      // sends all the content to texFile
      texFile.print(header)
      texFile.print(netLines)
      texFile.print(finalLines)

      // close file
      texFile.close()
   }

   /**
    * stores the header of the latex table
    * @return
    */
   def composeHeader() : String = {
      "\begin{table}[h!]\n" +
         "\\centering\n" +
         "\\begin{tabular}{|c|c|c|c|c|c|c|c|}\n" +
         "\\hline\n" +
         "network & $n$ & PT & PPT & VDG & VDI & IDP & IDS \\\\\\hline\n"
   }

   /**
    * compose a line with the savings for a net
    * @param net
    * @return
    */
   def composeLineForNet(net: String) : String = {
      // gets the measurements for the net (map with keys
      // representations and memory sizes
      val dataForNet = times.get(net).get

      // gets the base size: for TABLE
      val tableSize = dataForNet.get(ValueStoreTypes.TABLE).get

      // gets the number of tested indexes
      val indexes = numberIndexes.get(net).get

      // for the rest of storages compute the savings
      val completeReps = ValueStoreTypes.TABLE :: representations
      val timeReps = completeReps.map(rep => {
         val time = dataForNet.get(rep).get

         // compute saving value
         "%9.3f".format(time)
      }).mkString(" & ")

      // compose and return the line
      net + " & " + indexes + " & " + timeReps + "\\\\\\hline\n"
   }

   /**
    * compose the final declaration of data table
    * @return
    */
   def composeTableFinal(): String = {
      "\\end{tabular}\n" +
         "\\end{table}\n"
   }

   // call batch method of analysis
   //batchAnalysis(folder, extension)
   //generatePaperLatexTable
   override def main(args : Array[String]) = {
      println("net: " + args(0)+ " extension: " + args(1))
      val extension = args(1)
      var numberConfigurations : Long = 0L
      extension match{
         case "uai" => {
            numberConfigurations=20000
         }
         case "net" => {
            numberConfigurations=10000
         }
      }
      println(" numberConfigurations: " + numberConfigurations)
      singleAnalysis(args(0), extension, numberConfigurations)
   }
   //println(composeLineForNet(args(0) + extension))
}
