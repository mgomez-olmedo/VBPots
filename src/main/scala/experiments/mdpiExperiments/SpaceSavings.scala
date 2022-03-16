package experiments.mdpiExperiments

import bnet.Bnet
import potential.ValueStoreTypes

object SpaceSavings extends App{
   // stores the names of the networks and variables of interest
   val netsVariables: List[(String, List[String])] = List(
      ("hepar2.net", List("ggtp", "ast", "alt", "bilirubin")),
      ("diabetes.net", List("cho_0")),
      ("munin.net", List("L_MED_ALLCV_EW")),
      ("pathfinder.net", List("F74", "F40")))

   // for each pair net - variables consider sized for table, tree, pruned
   // tree, value driven with list of indices and index driven with pair of
   // arrays (values - (indices in potential, indices in array of values)
   // and value driven with maps
   val alternatives = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDILISTORE,
      ValueStoreTypes.IDPISTORE,
      ValueStoreTypes.IDMMSTORE)

   // print the header with the information
   println("net    variable     poss. values     TAB    TREE PRuNEDTREE  VDILI   IDPI   IDMM")
   for(pair <- netsVariables){
      // creates the net
      val bnet = Bnet(pair._1)

      // considers the potentials for the required variables
      for(variable <- pair._2){
         val potential = bnet.getPotentialForVariable(variable)

         // shows general info about the potential
         printf("%15s %15s %8d %10d", pair._1, variable,
               potential.store.getSize._1, potential.store.getMemorySize)

         // convert the potential to each one of the considered
         // alternatives
         for(alternative <- alternatives){
            val potConverted = potential.convert(alternative)

            // shows info about the potential
            printf("%10d", potConverted.store.getMemorySize)
         }
         println()
      }
   }
}
