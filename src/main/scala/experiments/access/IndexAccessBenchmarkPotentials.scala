package experiments.access

import experiments.generation.IndexesList
import org.scalameter.{Key, Warmer, config}
import potential.{Potential, ValueStoreTypes}
import utils.Util

import java.io.{File, PrintWriter}
import scala.collection.mutable.{ArrayBuffer, HashMap}

object IndexAccessBenchmarkPotentials extends App {
   // defines the folder for storing potentials
   val folder = "./data/potentialObjects/"

   // define the maximum cardinality values to consider
   //val maxCard = List(8000, 80000, 800000)
   val maxCard = List(8000, 80000)

   // defines the minimum global cardinality to consider
   //val minCard = List(4000, 40000, 400000)
   val minCard = List(4000, 40000)

   // define the number of different values to consider
   val levels = List(100, 1000)

   // define the number of potentials to generate for each combination
   val numberPotentials = 10

   // defines the list of representations to consider
   val representations = List(ValueStoreTypes.TABLE, ValueStoreTypes.TREE,
                              ValueStoreTypes.IDPMSTORE,
                              ValueStoreTypes.VDILMSTORE,
                              ValueStoreTypes.IDSMSTORE)

   // defines a data structure for storing a set of results for
   // a combination of maxCard-minCard and levels
   val times :  HashMap[(ValueStoreTypes.ValueStoreType, Int, Int), ArrayBuffer[Double]] = HashMap()

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 5,
      Key.exec.maxWarmupRuns -> 10,
      Key.exec.benchRuns -> 5,
      Key.verbose -> false
   ) withWarmer(new Warmer.Default)

   // considers all the combinations of parameters
   for(i <- 0 until 1){
      for(j <- 0 until levels.length){
         // compose the path to access
         val path = "./data/potentialObjects/"+maxCard(i)+"-"+minCard(i)+"-"+levels(j)+"/"

         // consider all rtepresentation types
         for(k <- 0 until representations.length){

            // store times for each potential
            val representationTimes:ArrayBuffer[Double] = ArrayBuffer.fill(numberPotentials)(0.0)

            // shows information about the process
            println("max card: " + maxCard(i), " min card: " + minCard(i) + " levels: " + levels(j) + " rep: " +
                        representations(k))

            // considers all the variants for a given combination and representation
            for(l <- 0 until numberPotentials){
               // set pots and indexes file names
               val filename = path + "pot-" + l + ".pot"
               val indexname = path + "ind-" +l + ".ind"

               // read potential and indexes
               var potential = Potential.readObject(filename)
               val indexes = IndexesList.readObject(indexname).indexes
               println("    potential real cardinality: " + potential.store.variables.possibleValues)

               // convert potential if required
               if(representations(k) != ValueStoreTypes.TABLE){
                  potential = potential.convert(representations(k))
               }

               // compute access time
               representationTimes(l) = (standardConfig measure {
                  for(j <- 0 until indexes.length){
                     potential.store.getValue(indexes(j))
                  }
               }).value

               // stores times for the corresponding combination
               times += ((representations(k), i, j) -> representationTimes)
            }

            // one a representation is complete, generate the
            // corresponding file
            println("saving result for " + maxCard(i)+ " - " + minCard(i) + " levels: " + levels(j))
            saveResults(i, j, k)
         }
      }
   }

   // now shows results for each set of potentials
   for(i <- 0 until maxCard.length){
      for(j <- 0 until levels.length){
         // gets data about table times (base ones)
         val tableData = times.get((ValueStoreTypes.TABLE, i, j)).get

         // gets statistics information for table representation
         val tableMean = Util.mean(tableData)
         val tableStd = Util.std(tableData)
         println("Table representation mean: " + tableMean + " std: " + tableStd)

         for(k <- 0 until representations.length){
            if(representations(k) != ValueStoreTypes.TABLE) {
               println("Data for potentials with global cardinality in (", maxCard(i), ", ",
                  minCard(i), " levels: ", levels(j), " representation: ", representations(k))
               val repData = times.get((representations(k), i, j)).get
               val mean = Util.mean(repData)
               val std = Util.std(repData)
               println("  mean: " + mean + " sd: " + std)
               println("  ratio wrt table: " + (mean / tableMean))
            }
         }
      }
   }

   /**
    * Saves the results for a group of potentials
    * @param indexCard
    * @param indexLevels
    * @param indexRep
    */
   def saveResults(indexCard : Int, indexLevels : Int,
                   indexRep : Int) = {
      // corresponding file
      val filename=representations(indexRep).toString + "-" + maxCard(indexCard) + "-" +
         minCard(indexCard) + "-" + levels(indexLevels)+ ".res"

      // creates the file in path
      val generalPath = "./data/potentialObjects/"
      val resFile = new PrintWriter(new File(generalPath + filename))

      // gets the times for this combination of parameters
      val timesRep = times.get((representations(indexRep), indexCard, indexLevels)) .get

      for(i <- 0 until timesRep.length){
         val potId = maxCard(indexCard)+"-"+minCard(indexCard)+"-"+levels(indexLevels)+"-"+i

         resFile.println(representations(indexRep).toString + ", " + potId + ", " +
            maxCard(indexCard) + ", " + minCard(indexCard) + ", " +
            levels(indexLevels) + ", " + timesRep(i))
      }

      // at the end, close the file
      resFile.close()
   }
}
