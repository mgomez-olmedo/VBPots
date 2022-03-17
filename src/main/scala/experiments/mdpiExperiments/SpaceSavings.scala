package experiments.mdpiExperiments

import bnet.Bnet
import inference.VariableElimination
import potential.{OperatorType, Potential, ValueDrivenStore, ValueStoreTypes}

object SpaceSavings extends App{
   // stores the names of the networks and variables of interest
   val netsVariables: List[(String, List[String])] = List(
      ("hepar2.net", List("ggtp", "ast", "alt", "bilirubin")),
      ("diabetes.net", List("cho_0")),
      ("munin.net", List("L_MED_ALLCV_EW")),
      ("pathfinder.net", List("F74", "F40")))

   // for each pair net - variables consider sizes for table, tree, pruned
   // tree, value driven with list of indices and index driven with pair of
   // arrays (values - (indices in potential, indices in array of values)
   // and value driven with maps
   val alternatives = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDILISTORE,
      ValueStoreTypes.IDPISTORE,
      ValueStoreTypes.IDMMSTORE)

   val alternativesWithPrune = List(
      ValueStoreTypes.VDILISTORE,
      ValueStoreTypes.IDPISTORE,
      ValueStoreTypes.IDMMSTORE)

   // list of thresholds to consider for the experiments
   val thresholds = List(0.00001, 0.00005,
                         0.0001,  0.0005,
                         0.001,   0.005,
                         0.01,    0.05, 0.1)

   /**
    * method generating general info about networks and variables
    * to consider
    */
   def generalInfo = {
      // print the header with the information
      println("net    variable     poss. values     TAB    TREE PRUNEDTREE  VDILI   IDPI   IDMM")
      for (pair <- netsVariables) {
         // creates the net
         val bnet = Bnet(pair._1)

         // considers the potentials for the required variables
         for (variable <- pair._2) {
            val potential = bnet.getPotentialForVariable(variable)

            // shows general info about the potential
            printf("%15s %15s %8d %10d", pair._1, variable,
               potential.store.getSize._1, potential.store.getMemorySize)

            // convert the potential to each one of the considered
            // alternatives
            for (alternative <- alternatives) {
               val potConverted = potential.convert(alternative)

               // shows info about the potential
               printf("%10d", potConverted.store.getMemorySize)
            }
            println()
         }
      }
   }

   /**
    * shows information about pruning reductions for a given set of
    * thresholds
    */
   def pruneInfo = {
      // shows the general header of the table
      print("net    variable")
      println()

      for (pair <- netsVariables){
         // creates the net
         val bnet = Bnet(pair._1)

         // gets the potentials for the required variables
         for (variable <- pair._2){
            printf("%15s %15s  ", pair._1, variable)
            println()
            val potential = bnet.getPotentialForVariable(variable)

            // shows the header for each alternative and sizes of
            // base representation and thresholds of pruning
            printf("      alternative     base-size ")
            thresholds.foreach(threshold => printf("%10.5f", threshold))
            println()

            // now perform pruning operations for each alternative
            for (alternative <- alternativesWithPrune){
               // makes the conversion
               val potentialConverted  = potential.convert(alternative)
               val store = potentialConverted.store.asInstanceOf[ValueDrivenStore]

               // gets the base size for the potential
               val baseSize = potentialConverted.store.getMemorySize
               printf("%15s %10d ", alternative, baseSize)

               // and considers each threshold
               for (threshold <- thresholds) {
                  val storePruned = store.prune(threshold)
                  val prunedSize = storePruned.getMemorySize
                  val savings = (prunedSize*100*1.0)/baseSize - 100
                  printf("%10.4f ", savings)
               }
               println()
            }
            println()
         }
      }
   }

   /**
    * gets info about making propagation with several thresholds for
    * pruning
    */
   def propagationInfo = {
      for (pair <- netsVariables){
         // creates the net
         val bnet = Bnet(pair._1)

         // makes a variable elimination engine
         val engine = new VariableElimination(bnet, false)

         // set default options for operations
         engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

         // considers each variable
         for (variable <- pair._2){
            // gets the base result without pruning
            val baseResult = engine.propagate(variable)

            // considers each representation with prune capabilities
            for(alternative <- alternativesWithPrune){
               print("alternative: " + alternative + "  ")
               // make the conversion of the net
               val convertedBnet = Bnet.convert(bnet, alternative)

               // makes the propagation with converted just for checking
               // results are coincident with base result
               val engineAlternative = new VariableElimination(convertedBnet, false)
               engineAlternative.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
               val altBaseResult = engineAlternative.propagate(variable)

               // computes KL distance between baseResult and altBaseResult
               val dist = baseResult.KLDistance(altBaseResult)
               println("base result: ")
               println(baseResult)
               println(" distance: " + dist)

               // now considers each threshold
               for(threshold <- thresholds){
                  // makes a new net for the threshold changing the potential
                  // of interest
                  val store = convertedBnet.getPotentialForVariable(variable).store.asInstanceOf[ValueDrivenStore]
                  val newStore = store.prune(threshold)
                  val newNet = convertedBnet.changePotentialForVariable(variable, newStore)

                  // makes a new engine
                  val engine = new VariableElimination(newNet, false)
                  engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
                  val result = engine.propagate(variable)

                  // computes the distance with respect to baseResult
                  val distAlt = baseResult.KLDistance(result)
                  println("       " + threshold + " distance: " + distAlt)
               }
            }
         }
      }
   }

   // call methods for producing results
   //generalInfo
   println("=====================================")
   //pruneInfo
   println("=====================================")
   propagationInfo
}
