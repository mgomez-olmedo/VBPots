package experiments

import bnet.Bnet
import potential.ValueStoreTypes

import java.nio.file.{Files, Paths}

/**
 * Object for making a general inspection on a network
 */
object BnetGeneralInfo extends App{
   // define the properties of the network to analyze
   val folder="./data/bnlearn/"
   val netName="pigs"
   val extension="net"

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

      // gets the top 10 biggest potentials
      val potsInfo = bnet.potentials.
         map(potential => (potential.mainVariable, potential.store.getSize)).
         sortBy(_._2._1).reverse.take(10)
      potsInfo.foreach(potInfo => {
         println("   variable: " + potInfo._1 + " max: " + potInfo._2._1 +
            " stored: " + potInfo._2._2 + " dif: "+ potInfo._2._3)
      })

      // gets the potential of maximum size
      val potMaxSize = bnet.potentials.maxBy(potential => potential.store.getSize._1)
      println("potential of max size: ")
      println("   variable: " + potMaxSize.mainVariable)
      println("   max number of values: " + potMaxSize.store.getSize._1)
      println("   number of stored values: " + potMaxSize.store.getSize._2)
      println("   number of different values: " + potMaxSize.store.getSize._3)
   }

   /**
    * Shows info about values patterns in potentials
    * @param bnet
    */
   def showValuesPatterns(bnet : Bnet) : Unit = {
      // iterates on potentials
      bnet.potentials.foreach(potential => {
         println("****************** " + potential.mainVariable + "******************")
         println(potential.convert(ValueStoreTypes.VDGLSTORE))
         println("Conditioning vars: " + potential.conditioningVars.mkString(" "))
         println("Possible values: " + potential.variables.possibleValues)
         println("Number of different values: " + potential.store.getDifferentValues.length)
         println("Top 10 most repetaed values: " + potential.store.getDifferentValues.take(10))
      })
   }

   /**
    * executes a single analysis on a net
    */
   val net = readNetData(folder, netName, extension)
   showBasicInfo(net)
   showValuesPatterns(net)
}
