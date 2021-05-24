package experiments.generation

import java.io.File

import potential.Potential

/**
 * Object for performing generation of pairs potential objects
 * according to max global cardinality, min global cardinality
 * and number of different values. The objects are generated
 * by pairs in order to include the same variable into their
 * domains
 */
object PotentialPairsGeneration extends App {

   // define the maximum cardinality values to consider
   val maxCard = List(1500, 8000, 16000, 80000, 180000, 800000)

   // defines the minimum global cardinality to consider
   val minCard = List(800, 4000, 8000, 40000, 90000, 400000)

   // define the number of different values to consider
   val levels = List(10, 50, 100, 500, 1000, 5000, 10000)

   // define the number of potentials to generate for each combination
   val potentials = 100

   // consider each combination of parameters
   for (i <- 0 until minCard.length) {
      println("minCard: " + minCard(i) + " maxCard: " + maxCard(i))
      for (j <- 0 until levels.length) {
         println("    levels: " + levels(j))
         for (k <- 0 until potentials) {
            // creates the potential
            val pair =
               Potential.generateRandomPotentialPair(5, minCard(i),
                  maxCard(i), levels(j))

            // store the pair
            storePair(pair._1, pair._2, maxCard(i), minCard(i),
                        levels(j), k)
         }
      }
   }

   /**
    * Creates a folder for storing potentials if needed
    *
    * @param potential1 potential to store
    * @param potential2 potential to store
    * @param max       max global cardinality
    * @param min       min global cardinality
    * @param levels    number of levels
    * @param id        id of potential
    */
   def storePair(potential1: Potential, potential2 : Potential,
                         max: Int, min: Int, levels: Int, id: Int) = {
      // defines the folder for storing potentials
      val folder = "./data/potentialObjects/"

      // creates the folder for the objects of the network
      // under analysis
      val path = folder + "comb/" + max + "-" +
                           min + "-" + levels + "/"
      val netFolder = new File(path)

      // creates the folder if needed
      if (!netFolder.exists()) {
         netFolder.mkdir()
      }

      // compose the name of the object
      var potFileName = path + "pot1-" + id + ".pot"

      // serialize Bnet object
      Potential.writeObject(potFileName, potential1)
      potFileName = path + "pot2-" + id + ".pot"
      Potential.writeObject(potFileName, potential2)
   }
}
