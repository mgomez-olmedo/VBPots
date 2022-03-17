package experiments.mdpiExperiments

import bnet.Bnet
import inference.VariableElimination
import potential.{OperatorType, ValueDrivenStore, ValueStoreTypes}

object GlobalSpaceSavings extends App{
   // stores the names of the networks and variables of interest
   val nets: List[String] = List(
      "hepar2.net", "diabetes.net", "munin.net", "pathfinder.net")

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
      println("net    TAB    TREE PRUNEDTREE  VDILI   IDPI   IDMM")
      for (netName <- nets) {
         // creates the net
         val bnet = Bnet(netName)

         // gets size for table representation
         val tabSize = bnet.getMemorySize

         // prints information about net and table size
         printf("%15s  %15d ", bnet.name, tabSize)

         // gets the global size of the net for each representation
         for(alternative <- alternatives){
            // convert the net
            val convertedBnet = Bnet.convert(bnet, alternative)

            // gets the size for the net
            val altSize = convertedBnet.getMemorySize
            printf("%15d ", altSize)
         }
         println

         // shows the header with alternative names
         for(alternative <- alternativesWithPrune){
            printf("%15s ", alternative)
         }
         println

         // now shows information about pruning
         // now considers each threshold
         for(threshold <- thresholds){
            printf("%15.5f -------------------------------", threshold)
            println

            // considers the alternative representations
            for(alternative <- alternativesWithPrune){
               // convert the net
               val convertedBnet = Bnet.convert(bnet, alternative)

               // perform the prune
               val prunedBnet = convertedBnet.prune(threshold)

               // shows information about size
               printf("  %15d ", prunedBnet.getMemorySize)
            }
            println
         }
      }
   }

   // call the method generating general info
   generalInfo
}
