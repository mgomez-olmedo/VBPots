package experiments.generation

import java.io.File

import potential.Potential

import scala.util.Random

/**
 * Object for performing generation of potential objects
 * according to max global cardinality, min global cardinality
 * and number of different values
 */
object PotentialsGeneration extends App {
   // defines the folder for storing potentials
   val folder = "./data/potentialObjects/"
   // define the maximum cardinality values to consider
   val maxCard = List(8000, 80000)//, 800000)

   // defines the minimum global cardinality to consider
   val minCard = List(4000, 40000)//, 400000)

   // define the number of different values to consider
   val levels = List(100, 1000)//, 10000)

   // define the number of potentials to generate for each combination
   val potentials = 20

   // consider each combination of parameters
   for (i <- 0 until minCard.length) {
      println("minCard: " + minCard(i) + " maxCard: " + maxCard(i))
      for (j <- 0 until levels.length) {
         println("    levels: " + levels(j))
         for (k <- 0 until potentials) {
            // creates the potential
            val potential =
               Potential.generateRandomPotential(5, minCard(i),
                  maxCard(i), levels(j), true)

            // generates the random set of index
            val indexes = generateRandomIndexesSet(potential)

            // stores potential and indexes
            storePotential(potential, indexes, minCard(i), maxCard(i),
               levels(j), k)
         }
      }
   }

   /**
    * generate a single potential to include in IJIS paper as
    * extreme case
    * @return
    */
   def generateIJISExample(percentage : Double) : Potential = {
      // force seed for reproducing the experiment
      scala.util.Random.setSeed(0)

      // generate the potential
      val potential = Potential.generateRandomPotential(4, 1024,
         1024, 2, true)

      // force a given percentage of zeros and return the result
      Potential.generateZeros(potential, percentage)
   }

   /**
    * generates a random set of indexes for testing the access to
    * potentials
    * @param potential potential to access
    */
   def generateRandomIndexesSet(potential : Potential) = {
      // gets the cardinality of the potential
      val maxCard = potential.variables.possibleValues

      // the number of indexes will be the same as maxCard
      (0L until maxCard).map(counter => Math.abs(Random.nextLong()) % maxCard).toList
   }

   /**
    * Creates a folder for storing potentials if needed
    *
    * @param potential potential to store
    * @param indexes set of indexes to store
    * @param max       max global cardinality
    * @param min       min global cardinality
    * @param levels    number of levels
    * @param id        id of potential
    */
   private def storePotential(potential: Potential, indexes : List[Long],
                              max: Int, min: Int, levels: Int, id: Int) = {
      // creates the folder for the objects of the network
      // under analysis
      val path = folder + "/" + min + "-" + max + "-" + levels + "/"
      val netFolder = new File(path)

      // creates the folder if needed
      if (!netFolder.exists()) {
         netFolder.mkdir()
      }

      // compose the name of the object
      val potFileName = path + "pot-" + id + ".pot"

      // compose the name of the object with the list of
      // indexes
      val indexesFileName = path + "ind-" + id + ".ind"

      // serialize Bnet object
      Potential.writeObject(potFileName, potential)

      // serialize the list
      IndexesList.writeObject(indexesFileName, new IndexesList(indexes))
   }
}
