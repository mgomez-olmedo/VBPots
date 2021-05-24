package experiments.combination

import java.io.{File, PrintWriter}

import org.scalameter.{Key, Warmer, config}
import potential.ValueStoreTypes.ValueStoreType
import potential.{OperatorType, Potential, ValueStoreTypes}
import utils.Util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Object for performing generation of pairs potential objects
 * according to max global cardinality, min global cardinality
 * and number of different values. The objects are generated
 * by pairs in order to include the same variable into their
 * domains
 */
object CombinationAAIMBenchmark extends App {
   // defines the folder for accessing potentials
   val folder = "./data/potentialObjects/comb/"

   // define the maximum cardinality values to consider
   val maxCard = List(1500, 8000, 16000)//, 80000, 180000, 800000)

   // defines the minimum global cardinality to consider
   val minCard = List(800, 4000, 8000)//, 40000, 90000, 400000)

   // define the number of different values to consider
   val levels = List(10, 50, 100, 500, 1000, 5000, 10000)

   // define the number of potentials to generate for each combination
   val numberPotentials = 100

   // defines a data structure for storing a set of results for
   // a combination of maxCard-minCard and levels
   val times :  mutable.HashMap[(ValueStoreTypes.ValueStoreType,
                        OperatorType.OperatorType, Int, Int),
                        ArrayBuffer[Double]] = mutable.HashMap()

   // defines the list of representations to consider
   val representations = List(ValueStoreTypes.TABLE, ValueStoreTypes.TREE,
                              ValueStoreTypes.IDPMUT)

   // consider the alternatives of combination function for AAIM
   val functions = List(OperatorType.DEFAULT, OperatorType.ALT1, OperatorType.ALT2,
                        OperatorType.ALT3, OperatorType.ALT4, OperatorType.ALT5)

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 10,
      Key.verbose -> false
   ) withWarmer new Warmer.Default

   // considers all the combinations of parameters
   for(i <- maxCard.indices) {
      for (j <- levels.indices) {
         println("--------- " + maxCard(i) + " " + minCard(i) + " " + levels(j) + " --------")
         println("Tables evaluation starting")
         // evaluates with tables
         tableEvaluation(i, j)

         println("Trees evaluation starting")
         // evaluate with trees
         treeEvaluation(i, j)

         println("aaim evaluation starting")
         // evaluate with AAIM and all its available functions for
         // combination
         aaimEvaluation(i, j)
      }
   }

   // now shows results for each set of potentials
   for(i <- maxCard.indices){
      for(j <- levels.indices){
         // shows info about maxcard and levels
         println("--------------------------------------------")
         println("Data for maxCard: " + maxCard(i) + " minCard: " + minCard(i) +
                  " levels: " + levels(j))
         println("--------------------------------------------")
         println("............... TABLE ......................")
         // gets data about table times (base ones)
         val tableData = times((ValueStoreTypes.TABLE, OperatorType.DEFAULT, i, j))

         // gets statistics information for table representation
         val tableMean = Util.mean(tableData)
         val tableStd = Util.std(tableData)
         println("Mean: " + tableMean + " std: " + tableStd)
         println("............................................")

         // shows information for tree
         println("............... TREE ......................")
         val treeData = times((ValueStoreTypes.TREE, OperatorType.DEFAULT, i, j))

         // gets statistics
         val treeMean = Util.mean(treeData)
         val treeStd = Util.std(treeData)
         println("Mean: " + treeMean + " std: " + treeStd)
         println("Ratio: " + (treeMean/tableMean))
         println("............................................")

         // shows information for AAIM and each combination function
         println("............... TREE ......................")

         for(i <- functions.indices){
            println("Function:  " + functions(i).toString)
            val aaimData = times((ValueStoreTypes.IDPMUT, functions(i), i, j))

            // gets statistics
            val aaimMean = Util.mean(aaimData)
            val aaimStd = Util.std(aaimData)
            println("Mean: " + aaimMean + " std: " + aaimStd)
            println("Ratio: " + (aaimMean/tableMean))
            println()
         }
      }
   }

   /**
    * execute combination with table representation
    * @param cardIndex index for cardinality
    * @param levelsIndex index for number of allowed levels
    */
   def tableEvaluation(cardIndex : Int, levelsIndex : Int): Unit = {
      // compose the path to access
      val path = folder + maxCard(cardIndex) + "-" + minCard(cardIndex) + "-" +
                     levels(levelsIndex) + "/"

      // store times for each potential
      val representationTimes:ArrayBuffer[Double] = ArrayBuffer.fill(numberPotentials)(0.0)

      // considers all the variants for a given combination and representation
      for(l <- 0 until numberPotentials){
         // set pots and indexes file names
         val filename1 = path + "pot1-" + l + ".pot"
         val filename2 = path + "pot2-" + l + ".pot"

         // read potential and indexes
         val potential1 = Potential.readObject(filename1)
         val potential2 = Potential.readObject(filename2)

         // set functions for combination and marginalization
         potential1.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

         // compute combination time
         representationTimes(l) = (standardConfig measure {
            potential1.combine(potential2)
         }).value
      }

      // stores times for the corresponding combination
      times += ((ValueStoreTypes.TABLE,
         OperatorType.DEFAULT, cardIndex, levelsIndex) -> representationTimes)

      // save results for tables
      saveResultsDefault(cardIndex, levelsIndex, ValueStoreTypes.TABLE)
   }

   /**
    * execute combination with tree representation
    * @param cardIndex index for cardinality
    * @param levelsIndex index for number of allowed levels
    */
   def treeEvaluation(cardIndex : Int, levelsIndex : Int) : Unit = {
      // compose the path to access
      val path = folder + maxCard(cardIndex) + "-" + minCard(cardIndex) + "-" +
         levels(levelsIndex) + "/"

      // store times for each potential
      val representationTimes:ArrayBuffer[Double] = ArrayBuffer.fill(numberPotentials)(0.0)

      // considers all the variants for a given combination and representation
      for(l <- 0 until numberPotentials){
         // set pots and indexes file names
         val filename1 = path + "pot1-" + l + ".pot"
         val filename2 = path + "pot2-" + l + ".pot"

         // read potential and indexes
         var potential1 = Potential.readObject(filename1)
         var potential2 = Potential.readObject(filename2)

         // convert to tree
         potential1 = potential1.convert(ValueStoreTypes.TREE)
         potential2 = potential2.convert(ValueStoreTypes.TREE)

         // set functions for combination and marginalization
         potential1.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

         // compute combination time
         representationTimes(l) = (standardConfig measure {
            potential1.combine(potential2)
         }).value
      }

      // stores times for the corresponding combination
      times += ((ValueStoreTypes.TREE,
         OperatorType.DEFAULT, cardIndex, levelsIndex) -> representationTimes)

      // save results for tables
      saveResultsDefault(cardIndex, levelsIndex, ValueStoreTypes.TREE)
   }

   /**
    * execute combination with aaim representation, considering all
    * available functions for combination
    * @param cardIndex index for cardinality
    * @param levelsIndex index for number of allowed levels
    */
   def aaimEvaluation(cardIndex : Int, levelsIndex : Int) : Unit = {
      // compose the path to access
      val path = folder + maxCard(cardIndex) + "-" + minCard(cardIndex) + "-" +
         levels(levelsIndex) + "/"

      // considers each available function for combination
      for(i <- functions.indices) {
         println("  function: " + functions(i).toString)
         // store times for each potential
         val representationTimes:ArrayBuffer[Double] = ArrayBuffer.fill(numberPotentials)(0.0)

         // considers all the variants for a given combination and representation
         for (j <- 0 until numberPotentials) {
            // set pots and indexes file names
            val filename1 = path + "pot1-" + j + ".pot"
            val filename2 = path + "pot2-" + j + ".pot"

            // read potential and indexes
            var potential1 = Potential.readObject(filename1)
            var potential2 = Potential.readObject(filename2)

            // convert to aaim
            potential1 = potential1.convert(ValueStoreTypes.IDPMUT)
            potential2 = potential2.convert(ValueStoreTypes.IDPMUT)

            // set functions for combination and marginalization
            potential1.setFunctions(functions(i), OperatorType.DEFAULT)

            // compute combination time
            representationTimes(j) = (standardConfig measure {
               potential1.combine(potential2)
            }).value
         }

         // stores times for the corresponding combination
         times += ((ValueStoreTypes.IDPMUT,
            functions(i), cardIndex, levelsIndex) -> representationTimes)
      }

      // save results for tables
      saveResultsFunctions(cardIndex, levelsIndex, ValueStoreTypes.IDPMUT)
   }

   /**
    * Saves the results for a group of potentials
    * @param cardIndex index for cardinality data
    * @param levelsIndex index for number of allowed levels
    * @param storeType type of storage to use
    */
   def saveResultsDefault(cardIndex : Int, levelsIndex : Int,
                          storeType : ValueStoreType) : Unit = {

      // corresponding file
      val filename="%s-%d-%d-%d.res".
         format(storeType.toString, maxCard(cardIndex),
                minCard(cardIndex), levels(levelsIndex))

      // creates the file in path
      val generalPath = "./data/potentialObjects/"
      val resFile = new PrintWriter(new File(generalPath + filename))

      // gets the times for this combination of parameters
      val timesRep = times((storeType, OperatorType.DEFAULT, cardIndex, levelsIndex))

      for(i <- timesRep.indices){
         val potId = maxCard(cardIndex)+"-"+minCard(cardIndex)+"-"+levels(levelsIndex)+"-"+i

         resFile.println(storeType.toString + ", " + OperatorType.DEFAULT+ ", " + potId +
            ", " + maxCard(cardIndex) + ", " + minCard(cardIndex) + ", " +
            levels(levelsIndex) + ", " + timesRep(i))
      }

      // at the end, close the file
      resFile.close()
   }

   /**
    * Saves the results for a group of potentials
    * @param cardIndex index of cardinal data
    * @param levelsIndex index of allowed levels
    * @param storeType type of storage to use
    */
   def saveResultsFunctions(cardIndex : Int, levelsIndex : Int,
                          storeType : ValueStoreType) : Unit = {

      // corresponding file
      val filename=storeType.toString + "-" + maxCard(cardIndex) + "-" +
         minCard(cardIndex) + "-" + levels(levelsIndex)+ ".res"

      // creates the file in path
      val generalPath = "./data/potentialObjects/"
      val resFile = new PrintWriter(new File(generalPath + filename))

      // considers all the functions
      for(i <- functions.indices) {
         val timesRep = times((storeType, functions(i), cardIndex, levelsIndex))
         for (j <- timesRep.indices) {
            val potId = maxCard(cardIndex) + "-" + minCard(cardIndex) + "-" + levels(levelsIndex) + "-" + j

            resFile.println(storeType.toString + ", " + functions(i).toString + ", " + potId +
               ", " + maxCard(cardIndex) + ", " + minCard(cardIndex) + ", " +
               levels(levelsIndex) + ", " + timesRep(j))
         }
      }

      // at the end, close the file
      resFile.close()
   }
}
