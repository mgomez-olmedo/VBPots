package experiments

import bnet.Bnet
import inference.VariableElimination
import potential.{Potential, ValueStoreTypes}

/**
  * Object extending of App for executing test about VariableElimination
  * algorithm execution
  */
object VariableEliminationTest extends App{
   /**
     * names of networks to propagate
     */
   val netNames = List("asia")

   /**
     * General extension for nets to analyze
     */
   val extension="net"

   /**
     * Complete list of bnet networks
     */
   val completeList =   List("alarm", "andes", "asia", "barley", "cancer", "child", "diabetes",
      "hailfinder", "hepar2", "insurance", "link", "mildew", "munin", "munin1", "munin2",
      "munin3", "munin4", "pathfinder", "pigs", "sachs", "survey", "water", "win95pts")

   /**
     * List of representations to consider for evaluation
     */
   val methods: List[ValueStoreTypes.Value] = List(ValueStoreTypes.TREE)

   // consider each net stored in netNames
   for (netName <- netNames) {
      println("net: " + netName)
      val bnet = Bnet(netName + "." + extension)
      println()
      for (method <- methods) {
         // creates the engine for the corresponding representation
         val convertedBnet = Bnet.convert(bnet, method)
         println("  method: " + method )
         println("  ...............................................")
         val engine = new VariableElimination(convertedBnet, true)

         // check the time required for VE with tables
         val initialTime = System.currentTimeMillis()
         val results: List[Potential] = engine.propagate
         val finalTime = System.currentTimeMillis()

         // shows the results
         println()
         println("----------------- results of propagation ------------------")
         //results.foreach(potential => println(potential))
         //println(engine.stats.potSizes)
      }
   }
}
