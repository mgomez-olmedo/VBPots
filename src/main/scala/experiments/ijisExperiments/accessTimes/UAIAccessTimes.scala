package experiments.ijisExperiments.accessTimes

import experiments.access.IndexAccessBnetSelectBenchmark
import experiments.serializeNets.NetSerializator

/**
 * Object for generating the results for access times experiment
 * for Bnlearn networks
 */
object UAIAccessTimes extends App {
  // selected representations are already fixed in class
  // IndexAccessBnetSelectBenchmark

  // set of nets to analyze
  val nets = List("BN_76.uai", "BN_87.uai", "BN_29.uai", "BN_125.uai",
    "BN_115.uai", "BN_119.uai", "BN_121.uai", "BN_123.uai", "BN_27.uai",
    "BN_117.uai", "BN_22.uai", "BN_111.uai", "BN_109.uai", "BN_113.uai",
    "BN_20.uai", "BN_10.uai7", "BN_105.uai")

  // set the number of configurations (indexes) to access
  // for bnlearn networks
  val numberConfigurations = 10000L

  // makes serialization if needed. This makes computation time for the
  // experiment shorter when serialized versions of networks are available
  nets.foreach(net => NetSerializator.serializeNet(net,
    IndexAccessBnetSelectBenchmark.representations))

  // shows the header with information about types of interest
  val reps = "net & " + "TABLE  & " + IndexAccessBnetSelectBenchmark.representations.mkString(" & ")
  println(reps)
  for(net <- nets) {
    IndexAccessBnetSelectBenchmark.singleAnalysis(net, numberConfigurations)
  }
}