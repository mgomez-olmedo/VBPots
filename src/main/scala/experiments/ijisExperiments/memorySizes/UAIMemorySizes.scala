package experiments.ijisExperiments.memorySizes

import experiments.{BnetAnalysis, ExperimentConfiguration}
import potential.ValueStoreTypes

/**
 * generates the complete table for the results of memory space
 * required for bnlearn networks
 */
object UAIMemorySizes extends App{
   // sets types of interest
   ExperimentConfiguration.typesOfInterest = List(
      ValueStoreTypes.TABLE,
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      //ValueStoreTypes.MAPSETGRAIN,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTIMMUT,
      //ValueStoreTypes.MAPSETINDICESIMMUT,
      ValueStoreTypes.IDPIMMUT,
      // ValueStoreTypes.IDSETIMMUT,
      ValueStoreTypes.IDMMUT)

   // sets the list of nets to examine
   val nets = List("BN_76.uai", "BN_87.uai", "BN_29.uai",
      "BN_125.uai", "BN_115.uai", "BN_119.uai",
      "BN_121.uai", "BN_123.uai", "BN_27.uai",
      "BN_117.uai", "BN_22.uai", "BN_111.uai",
      "BN_109.uai", "BN_113.uai", "BN_20.uai",
      "BN_107.uai", "BN_105.uai"
   )

   // perform the analysis of networks one by one
   // shows the header with information about types of interest
   val reps = "net & " + ExperimentConfiguration.typesOfInterest.drop(1).mkString(" & ")
   println(reps)
   nets.foreach(net => {
      // print the line for the net analyzed
      print(BnetAnalysis.singleAnalysis(net, "./data/UAI/"))
   })
}
