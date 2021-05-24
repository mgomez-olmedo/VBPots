package experiments

import bnet.Bnet
import potential.ValueStoreTypes.ValueStoreType
import potential.{Potential, ValueStoreTypes}

import scala.collection.mutable.Map

/**
  * Class for inspecting the result of conversions and relations
  * between several representations.
  * This class is used for getting the data contained in the
  * paper and representing the specific features of some of
  * the representations of potentials as well as general
  * features of networks
  * @param netName name of the net of interest
  * @param extension type of net (bnlearn or uai)
  * @param variableName name of the variable on interest
  * @param storeTypes types of storage to analyze
  */
class BnetPotentialInspection(val netName : String,
                              val extension : String,
                              val variableName : String,
                              val storeTypes:List[ValueStoreType]) {

   // bnet created from the fileName passed as argument
   val bnet: Bnet = Bnet(netName + "." + extension)

   // stores info about potentials
   val potentialRepresentations: Map[ValueStoreType, Potential] =
      Map[ValueStoreType, Potential]()
   val globalSizes : Map[ValueStoreType, Long] =
      Map[ValueStoreType, Long]()
   val potentialsInfo : Map[ValueStoreType, PotentialInfo] =
      Map[ValueStoreType, PotentialInfo]()

   /**
     * Computes the information about potentials representation
     */
   def generateInfo : Unit = {
      // considers each possible representation
      storeTypes.foreach(storeType => generateInfo(storeType))
    }

   /**
     * Prints a certain representation of the potential for
     * a variable
     * @param storeType type of storage to consider
     */
   def printPotential(storeType : ValueStoreType) = {
      val potential: Potential =
         potentialRepresentations.get(storeType).get

      // shows the info about the potential
      println(potential.store)

      // show the rest of information
      val info: PotentialInfo = potentialsInfo.get(storeType).get
      println("indices: " + info.sizes._1 + " values: " + info.sizes._2 +
         " diff. values: " +  info.sizes._3)
      println("memory size: " + info.memorySize + " object size: " +
         info.serializationSize)

      // gets additional information
      val additionalInfo = potential.getPatternMeasures
      println("default value: " + additionalInfo._2, " repetitions: " +
         additionalInfo._3 + " max prop: " + additionalInfo._4)
      println("aver prop: " + additionalInfo._5 + " min prop: " +
         additionalInfo._6)
      println("changes: " + additionalInfo._7)
   }

   def printGlobalSizes = {
      println("Global sizes for representations of interest----------")
      for(entry <- globalSizes){
         println(entry._1 + " : " + entry._2)
      }
      println("-------------------------------------------")
   }

   /**
     * Computes the information about a concrete type of
     * representation
     * @param storeType type of storage of interest
     */
   private def generateInfo(storeType: ValueStoreTypes.Value):Unit = {
      // convert the net to the required type
      val bnetConverted = Bnet.convert(bnet, storeType)

      // gets the potential for the variable
      val potential = bnetConverted.getPotentialForVariable(variableName)

      // stores the info into potentials
      globalSizes.put(storeType, bnetConverted.getPotentialsMemorySize)
      potentialRepresentations.put(storeType, potential)
      // TODO: the last parameter avoids to generate serialization info
      val folder = "./analysis/"
      potentialsInfo.put(storeType, new PotentialInfo(folder,
         netName, potential, false))
   }
}

/**
  * Companion object extending app for performing the
  * experiment
  */
object BnetPotentialInspection extends App{
   /**
    * List of alternatives to examine
    */
   val listAlternatives = List(ValueStoreTypes.TABLE,
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLIST,
      ValueStoreTypes.VDILISTIMMUT,
      ValueStoreTypes.IDPIMMUT,
      ValueStoreTypes.IDMMUT)

   /**
     * creates an object of inspector class
     * The last argument is a list of types: in this case a single
     * one
     */
   val inspector = new BnetPotentialInspection("barley",
                     "net", "ngodn",
                     listAlternatives)

   // call generateInfo method
   inspector.generateInfo

   // print global sizes for all the potentials of the net
   inspector.printGlobalSizes

   // shows all the representations
   for(representation <- listAlternatives) {
      println("------------------------------------------")
      println(representation)
      inspector.printPotential(representation)
      println("------------------------------------------")
   }
}
