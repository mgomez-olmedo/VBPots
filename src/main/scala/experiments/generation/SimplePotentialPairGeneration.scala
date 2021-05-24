package experiments.generation

import potential.Potential

/**
 * generates a simple pair of potentials
 */
object SimplePotentialPairGeneration extends App{
   // defines the folder for storing potentials
   val folder = "./data/potentialObjects/comb/"

   // define the maximum cardinality values to consider
   val maxCard = 1500

   // defines the minimum global cardinality to consider
   val minCard = 800

   // define the number of different values to consider
   val levels = 50

   // define the number of potentials to generate for each combination
   val potentials = 1

   // generate the pair
   val pair = Potential.generateRandomPotentialPair(5, minCard, maxCard, levels)

   // store the pair
   PotentialPairsGeneration.storePair(pair._1, pair._2, maxCard, minCard, levels, 0)

   val filename1 = folder + maxCard + "-" + minCard + "-" + levels + "/pot1-" + "0.pot"
   val filename2 = folder + maxCard + "-" + minCard + "-" + levels + "/pot2-" + "0.pot"
   var potential1 = Potential.readObject(filename1)
   var potential2 = Potential.readObject(filename2)

   // shows information about variables before and after reading
   println("potentials before serialization..................")
   println(pair._1.variables.simpleToString)
   println(pair._1.variables.map(_.hashCode()).mkString(" "))
   println(pair._2.variables.simpleToString)
   println(pair._2.variables.map(_.hashCode()).mkString(" "))
   println("potentials after serialization..................")
   println(potential1.variables.simpleToString)
   println(potential1.variables.map(_.hashCode()).mkString(" "))
   println(potential2.variables.simpleToString)
   println(potential2.variables.map(_.hashCode()).mkString(" "))
}
