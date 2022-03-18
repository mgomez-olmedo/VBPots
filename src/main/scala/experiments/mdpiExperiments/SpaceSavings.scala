package experiments.mdpiExperiments

import bnet.Bnet
import inference.VariableElimination
import potential.ValueStoreTypes.ValueStoreType
import potential.{OperatorType, Potential, ValueDrivenStore, ValueStoreTypes}

import scala.collection.mutable

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
            alternativesWithPrune.foreach(alternative => printf("%10s", alternative))
            println

            // store bases sizes for each representation
            val baseSizes = mutable.Map[ValueStoreType, Long]()

            // first at all considers the version without pruning
            for(alternative <- alternativesWithPrune){
               // makes the conversion
               val potentialConverted  = potential.convert(alternative)
               val store = potentialConverted.store.asInstanceOf[ValueDrivenStore]

               // gets the base size for the potential
               val baseSize = potentialConverted.store.getMemorySize
               baseSizes += (alternative -> baseSize)
               printf("%10d ", baseSize)
            }
            println

            // no consider each threshold
            for(threshold <- thresholds){
               // shows info about the threshold
               printf("%8.5f ", threshold)

               // now consider the alternatives with pruning capabilities
               for(alternative <- alternativesWithPrune){
                  // makes the conversion
                  val potentialConverted  = potential.convert(alternative)
                  val store = potentialConverted.store.asInstanceOf[ValueDrivenStore]

                  // makes the prune
                  val storePruned = store.prune(threshold)
                  val prunedSize = storePruned.getMemorySize
                  val savings = (prunedSize*100.0)/baseSizes(alternative) - 100
                  printf("%10.4f ", savings)
               }
               println
            }
            println
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
         print("net    variable")
         println

         // makes a variable elimination engine
         val engine = new VariableElimination(bnet, false)

         // set default options for operations
         engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

         // considers each variable
         for (variable <- pair._2){
            // shows info about net and variable
            printf("%15s %15s ", pair._1, variable)
            println

            // gets the base result without pruning
            val baseResult = engine.propagate(variable)

            // shows information about alternatives
            for(alternative <- alternativesWithPrune){
               printf("%15s ", alternative)
            }
            println

            // considers each threshold
            for(threshold <- thresholds){
               // shows info about the threshold
               printf("%8.5f ", threshold)

               // considers each alternative
               for(alternative <- alternativesWithPrune){
                  // make the conversion of the net
                  val convertedBnet = Bnet.convert(bnet, alternative)

                  // gets the potential of interest and perform the
                  // prune
                  val store = convertedBnet.getPotentialForVariable(variable).store.asInstanceOf[ValueDrivenStore]
                  val newStore = store.prune(threshold)
                  val newNet = convertedBnet.changePotentialForVariable(variable, newStore)

                  // makes a new engine
                  val engine = new VariableElimination(newNet, false)
                  engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
                  val result = engine.propagate(variable)

                  // computes the distance with respect to baseResult
                  val distance = baseResult.KLDistance(result)

                  // shows info about the distance
                  printf("%8.5f ", distance)
               }
               println
            }
            println
         }
      }
   }

   // call methods for producing results
   generalInfo
   println("=====================================")
   pruneInfo
   println("=====================================")
   propagationInfo
   println("=====================================")
}
