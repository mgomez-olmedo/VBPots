package experiments.serializeNets

import bnet.Bnet
import potential.ValueStoreTypes

import java.nio.file.{Files, Paths}
import java.util.Calendar

object NetSerializator extends App{

   /**
    * Serialize a representation for a given file in order to simplify
    * experiments with access times avoiding conversions
    * @param fileName
    * @param representation
    */
   def serializeNet(fileName : String, representation : ValueStoreTypes.ValueStoreType) : Unit = {
      // extract components of fileName
      val components = utils.Util.separateExtension(fileName)
      val netName = components._1
      val extension = components._2

      // compose folder for the object with serialized information
      // it is stored in a specific folder for each network
      val folder = extension match{
         case "net" => "./data/bnlearn/" + netName + "/"
         case "uai" =>  "./data/UAI/" + netName + "/"
      }

      // compose the name for the serialized object
      val objectFileName =
         folder + netName + "-obj-" + representation.toString + "." + extension

      // check if the folder and files are already present
      val folderReady = Files.exists(Paths.get(folder))
      val fileReady = if(folderReady){
         // checks its presence in the corresponding folder
         Files.exists(Paths.get(objectFileName))
      }
      else false

      // if the folder is not ready, creates it
      if(folderReady == false){
         Files.createDirectory(Paths.get(folder))
      }

      // if the file is not present, make the conversion and store the
      // object
      if(fileReady == false) {
         val bnet = Bnet(fileName)
         val converted = Bnet.convert(bnet, representation)

         // store the object
         Bnet.writeObject(objectFileName, converted)
      }
   }

   /**
    * serialize, if needed, a net for a given list of representation
    * @param fileName
    * @param representation
    */
   def serializeNet(fileName : String, representations : List[ValueStoreTypes.ValueStoreType]): Unit = {
      representations.foreach(representation => serializeNet(fileName, representation))
   }

   /**
    * Serialize a representation for a given file in order to simplify
    * experiments with access times avoiding conversions
    * @param fileName
    * @param representation
    */
   def readSerializedNet(fileName : String, representation : ValueStoreTypes.ValueStoreType) : Bnet = {
      // extract components of fileName
      val components = utils.Util.separateExtension(fileName)
      val netName = components._1
      val extension = components._2

      // compose folder for the object with serialized information
      // it is stored in a specific folder for each network
      val folder = extension match{
         case "net" => "./data/bnlearn/" + netName + "/"
         case "uai" =>  "./data/UAI/" + netName + "/"
      }

      // compose the name for the serialized object
      val objectFileName =
         folder + netName + "-obj-" + representation.toString + "." + extension

      // check if the folder and files are already present
      val folderReady = Files.exists(Paths.get(folder))
      val fileReady = if(folderReady){
         // checks its presence in the corresponding folder
         Files.exists(Paths.get(objectFileName))
      }
      else false

      // read object if everything is ok
      if(folderReady && fileReady){
         Bnet.readObject(objectFileName)
      }
      else {
         println("Error reading serialized version of potential")
         println("required serialization of network")
         null
      }
   }

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
         // make conversion
         println("making conversion of " + netName + " for " + representation.toString)
         //val convertedBnet = Bnet.convert(bnet, representation)
         serializeNet(fileName, representation)
         // serialize object
         //Bnet.writeObject(netName + "-obj-" + representation.toString + "." + extension, convertedBnet)
      })
   })
}
