package experiments.ijisExperiments.accessTimes

import experiments.access.IndexAccessBnetSelectBenchmark
import experiments.serializeNets.SerializeNets

/**
 * Object for generating the results for access times experiment
 * for Bnlearn networks
 */
object BnlearnAccessTimes extends App{
  // selected representations are already fixed in class IndexAccessBnetSelectBenchmark

  // sets the list of nets to examine
  val nets = List("cancer.net", "asia.net", "survey.net", "sachs.net",
    "child.net", "alarm.net", "win95pts.net", "insurance.net", "hepar2.net",
    "andes.net", "hailfinder.net", "pigs.net", "water.net", "munin1.net",
    "link.net", "munin2.net", "munin3.net", "pathfinder.net", "munin4.net",
    "munin.net", "barley.net", "diabetes.net", "mildew.net")

  // set the number of configurations (indexes) to access
  // for bnlearn networks
  val numberConfigurations = 10000

  // makes serialization if needed. This makes computation time for the
  // experiment shorter when serialized versions of networks are available
  nets.foreach(net => SerializeNets.serializeNet(net,
                                        IndexAccessBnetSelectBenchmark.representations))

  // shows the header with information about types of interest
  val reps = "net & " + IndexAccessBnetSelectBenchmark.representations.mkString(" & ")
  println(reps)
  nets.foreach(net => {
    (0 until 10).foreach(index => {
       IndexAccessBnetSelectBenchmark.singleAnalysis(net, numberConfigurations)
    })
  })
}
