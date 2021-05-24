package experiments.serializeNets

import bnet.Bnet
import potential.ValueStoreTypes

import java.util.Calendar

object SerializeNets extends App{

   // sets net name
   // sets net name
   val netNames = List("cancer", "asia", "survey", "sachs", "child",
            "alarm", "win95pts", "insurance", "hepar2", "andes",
           "hailfinder", "pigs", "water", "munin1", "link",
            "munin2", "munin3", "pathfinder", "munin4", "munin",
            "barley", "diabetes", "mildew")
   //val netNames = List("BN_76", "BN_87", "BN_29", "BN_125",
   //      "BN_115", "BN_119", "BN_121", "BN_123", "BN_27",
   //      "BN_117", "BN_22", "BN_111", "BN_109", "BN_113",
   //      "BN_20", "BN_107")
   // val netNames = List("BN_117")

   // extension
   val extension = "net"

   // consider each net
   netNames.par.foreach(netName => {
      // create Bnet object reading from file
      println("Reading file")
      println("time: " + Calendar.getInstance().getTime())
      val bnet = Bnet(netName + "." + extension)

      // convert it to the desired representations
      val representations = List(
         ValueStoreTypes.TABLE,
         ValueStoreTypes.TREE,
         ValueStoreTypes.PRUNEDTREE,
         ValueStoreTypes.VDGLIST,
         ValueStoreTypes.VDILISTMUT,
         ValueStoreTypes.IDPMUT,
         ValueStoreTypes.IDSETMUT,
         ValueStoreTypes.IDMMUT)

      // makes conversions and store files
      representations.foreach(representation => {
         // make conversion
         println("making conversion of " + netName + " for " + representation.toString)
         println("time: " + Calendar.getInstance().getTime())
         val convertedBnet = Bnet.convert(bnet, representation)

         // serialize object
         println("writing serialized version of " + netName + " for " + representation.toString)
         println("time: " + Calendar.getInstance().getTime())
         Bnet.writeObject(netName + "-obj-" + representation.toString + "." + extension, convertedBnet)
         println("finished writing of serialized version of " + netName + " for " + representation.toString)
      })
   })
}
