package experiments

import bnet.Bnet

import java.nio.file.{Files, Paths}

object BnetGeneralInfo extends App{
   // define the properties of the network to analyze
   val folder="./data/UAI/selected/"
   val netName="BN_109"
   val extension="uai"

   /**
    * Read the data of a net
    * @param folder target folder
    * @param name name of target network
    * @param extension corresponding extension
    * @return
    */
   def readNetData(folder : String, name : String, extension : String) : Bnet = {
      // compose file name of serialized object
      val filename = netName + "-obj-" + "TABLE." + extension

      // check if object exists or read the file with netork
      // description in other case
      println("Checking path for " + (folder + filename))
      if (Files.exists(Paths.get(folder + filename))) {
         try {
            Bnet.readObject(filename)
         }
         catch {
            case e: Exception => {
               println("Error while reading serialized object")
               println("reading from text description and parsing")
               Bnet(netName + "." + extension)
            }
         }
      }
      else {
         Bnet(netName + "." + extension)
      }
   }

   /**
    * shows basic info of the network passed as argument
    * @param bnet
    */
   def showBasicInfo(bnet : Bnet) : Unit = {
      // get general info about the network of interest
      // gets a tuple with number of variables, number of arcs,
      // global number of states,
      // avg number of states, min number of states and max number of
      // states
      val info = bnet.getGeneralInfo
      println("number of variables: " + info._1)
      println("number of arcs: " + info._2)
      println("global number of states: " + info._3)
      println("min number of states: " + info._5)
      println("avg number of states: " + info._4)
      println("max number of states: " + info._6)

      // gets the global number of parameters
      val numberParameters = bnet.potentials.
         map(potential => potential.variables.possibleValues).sum
      println("global number of parameters: " + numberParameters)
   }

   /**
    * Shows info about values patterns in potentials
    * @param bnet
    */
   def showValuesPatterns(bnet : Bnet) : Unit = {
      // iterates on potentials
      bnet.potentials.foreach(potential => {
         println("****************** " + potential.mainVariable + "******************")
         println("Conditioning vars: " + potential.conditioningVars.mkString(" "))
         println("Possible values: " + potential.variables.possibleValues)
         println("Number of different values: " + potential.store.getDifferentValues.length)
         println("Top 10 most repetaed values: " + potential.store.getDifferentValues.take(10))
      })
   }
}
