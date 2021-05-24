package experiments.ijisExperiments

import experiments.{BnetAnalysis, ExperimentConfiguration}
import potential.ValueStoreTypes

/**
 * generates the complete table for the results of memory space
 * required for bnlearn networks
 */
object BnlearnMemorySizes extends App{
   // sets types of interest
   ExperimentConfiguration.typesOfInterest = List(
      ValueStoreTypes.TABLE,
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      //ValueStoreTypes.MAPSETGRAIN,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTIMMUT,
      //ValueStoreTypes.MAPSETINDICESIMMUT,
      ValueStoreTypes.IDPMUT,
      // ValueStoreTypes.IDSETIMMUT,
      ValueStoreTypes.IDMMUT)

   // sets the list of nets to examine
   val nets = List("cancer.net", "asia.net", "survey.net", "sachs.net",
      "child.net", "alarm.net", "win95pts.net", "insurance.net", "hepar2.net",
      "andes.net", "hailfinder.net", "pigs.net", "water.net", "munin1.net",
      "link.net", "munin2.net", "munin3.net", "pathfinder.net", "munin4.net",
      "munin.net", "barley.net", "diabetes.net", "mildew.net")

   // perform the analysis of networks one by one
   // shows the header with information about types of interest
   val reps = "net & " + ExperimentConfiguration.typesOfInterest.drop(1).mkString(" & ")
   println(reps)
   nets.foreach(net => {
      // print the line for the net analyzed
      print(BnetAnalysis.singleAnalysis(net, "./data/bnlearn/"))
   })
}
