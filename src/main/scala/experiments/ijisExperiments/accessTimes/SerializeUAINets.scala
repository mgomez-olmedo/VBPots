package experiments.ijisExperiments.accessTimes

import bnet.Bnet
import experiments.serializeNets.NetSerializator.serializeNet
import potential.ValueStoreTypes

import java.util.Calendar

object SerializeUAINets extends App{
   // sets net name
   val netNames = List("BN_76", "BN_87", "BN_29", "BN_125",
         "BN_115", "BN_119", "BN_121", "BN_123", "BN_27",
         "BN_117", "BN_22", "BN_111", "BN_109", "BN_113",
         "BN_20", "BN_107", "BN_105")

   // extension
   val extension = "uai"

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
