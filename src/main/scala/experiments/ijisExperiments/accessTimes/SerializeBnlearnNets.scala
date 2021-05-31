package experiments.ijisExperiments.accessTimes

import bnet.Bnet
import experiments.serializeNets.NetSerializator.serializeNet
import potential.ValueStoreTypes

import java.util.Calendar

object SerializeBnlearnNets extends App{
   // sets net name
   val netNames = List("cancer", "asia", "survey", "sachs", "child",
      "alarm", "win95pts", "insurance", "hepar2", "andes",
      "hailfinder", "pigs", "water", "munin1", "link",
      "munin2", "munin3", "pathfinder", "munin4", "munin",
      "barley", "diabetes", "mildew")

   // extension
   val extension = "net"

   // consider each net
   netNames.foreach(netName => {
      // create Bnet object reading from file
      println("Reading file for " + netName)
      println("time: " + Calendar.getInstance().getTime())
      val fileName = netName + "." + extension
      val bnet = Bnet(fileName)

      // convert it to the desired representations
      val representations = List(
         ValueStoreTypes.TABLE,
         ValueStoreTypes.TREE,
         ValueStoreTypes.PRUNEDTREE,
         ValueStoreTypes.VDGLSTORE,
         ValueStoreTypes.VDILMSTORE,
         ValueStoreTypes.IDPMSTORE,
         ValueStoreTypes.IDSMSTORE,
         ValueStoreTypes.IDMMSTORE)

      // makes conversions and store files
      representations.foreach(representation => {
         // make conversion and serialize
         println("making conversion of " + netName + " for " + representation.toString)
         serializeNet(fileName, representation)
      })
   })
}
