package experiments.ijisExperiments.accessTimes

import experiments.access.IndexAccessBnetSelectBenchmark

/**
 * Object for generating the results for access times experiment
 * for Bnlearn networks
 */
object NetAccessTimes{
  /**
   * main method for execution. It requires nets be serialized and to specify
   * the name of the bnlearn network to use
   * @param args
   */
  def main(args : Array[String]) = {
    if(args.length < 2){
      println("Error using program. Required name of bnlearn net to use and number of indexes")
      println("Example: alarm.net 10000")
      System.exit(-1)
    }
    else{
      val numberConfigurations = args(1).toLong
      println("Number of access: " + numberConfigurations)
      val reps = "net & " + "TABLE  & " + IndexAccessBnetSelectBenchmark.representations.mkString(" & ")
      IndexAccessBnetSelectBenchmark.singleAnalysis(args(0), numberConfigurations)
    }
  }
}