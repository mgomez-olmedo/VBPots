package experiments.mdpiExperiments

import bnet.Bnet
import inference.VariableElimination
import potential.ValueStoreTypes.ValueStoreType
import potential.{OperatorType, Potential, ValueDrivenStore, ValueStoreTypes}

import scala.collection.mutable

object GlobalSpaceSavings extends App{
   // stores the names of the networks and variables of interest
   val nets: List[String] = List(
      "hepar2.net", "diabetes.net", "munin.net", "pathfinder.net")

   val variables : Map[String, List[String]] = Map(
      "hepar2.net" -> List("ggtp", "ast", "alt", "bilirubin"),
      "diabetes.net" -> List("cho_0", "cho_1", "cho_2"),
      "munin.net" -> List("L_LNLPC5_DELT_MUSIZE", "L_LNLE_ADM_MUSIZE", "L_MED_ALLCV_EW"),
      "pathfinder.net" -> List("F39", "F74", "F40")
   )

   // for each pair net - variables consider sizes for table, tree,
   // pruned tree, value driven with list of indices and index driven
   // with pair of arrays (values - (indices in potential, indices in
   // array of values) and value driven with maps
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

   // defines the base alternative for making the propagations
   // after pruning the whole set of potentials
   val baseAlternative = ValueStoreTypes.VDILISTORE

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
         val baseSizes : mutable.Map[ValueStoreTypes.Value, Long] = mutable.Map[ValueStoreTypes.Value, Long]()
         for(alternative <- alternatives){
            // convert the net
            val convertedBnet = Bnet.convert(bnet, alternative)

            // gets the size for the net and store it for computing
            // posterior savings
            val altSize = convertedBnet.getMemorySize
            baseSizes += (alternative -> altSize)
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
            printf("%8.5f ", threshold)

            // considers the alternative representations
            for(alternative <- alternativesWithPrune){
               // convert the net
               val convertedBnet = Bnet.convert(bnet, alternative)

               // perform the prune
               val prunedBnet = convertedBnet.prune(threshold)

               // gets the base size for this representation
               val baseSize = baseSizes.get(alternative).get
               val altSize = prunedBnet.getMemorySize
               val saving = (altSize*100.0)/baseSize - 100

               // shows information about size and saving
               printf("  %10d (%7.4f) ", prunedBnet.getMemorySize, saving)
            }
            println
         }
      }
   }

   /**
    * shows information about performing propagation
    * on the selected variables after pruning all the
    * potentials with the set of thresholds
    */
   def propagationInfo = {
      // considers each net
      for(netName <- nets){
         // creates the net
         val bnet = Bnet(netName)
         // shows information about the net name
         printf("%15s ----------------------------------\n", netName)

         // creates an engine for propagation and obtaining the base
         // potential of exact result
         val engine = new VariableElimination(bnet, false)
         engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

         // makes a propagation for each variable and store the results
         val baseResults : mutable.Map[String, Potential] = mutable.Map()
         for(variable <- variables(netName)){
            // makes a propagation for the variable
            val result = engine.propagate(variable)
            baseResults += (netName+variable -> result)

            // shows info about the variable name
            printf("%15s ", variable)
         }
         println

         // considers now the set of thresholds
         for(threshold <- thresholds){
            // shows info about threshold
            printf("%8.5f ", threshold)

            // make the general prune over the whole set of potentials
            val prunedNet = Bnet.convert(bnet, baseAlternative).prune(threshold)

            // makes a engine for propagation
            val prunedEngine = new VariableElimination(prunedNet, false)
            prunedEngine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

            // makes a propagation for each variable
            for(variable <- variables(netName)){
               val prunedResult = prunedEngine.propagate(variable)

               // compute the KL distance for the threshold
               val distance = baseResults(netName+variable).KLDistance(prunedResult)

               // shows info about about the distance
               printf("%8.5f ", distance)
            }
            println
         }
      }
   }

   // call the method generating general info
   println("=====================================")
   generalInfo
   println("=====================================")
   propagationInfo
   println("=====================================")
}
