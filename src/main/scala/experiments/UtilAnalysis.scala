package experiments

import java.io.File

import base.{Variable, VariableSet}
import bnet.Bnet
import potential.Potential
import potential.ValueStoreTypes.ValueStoreType

import scala.collection.mutable.Map

/**
  * Object offering utility function for bnet analysis
  */
object UtilAnalysis {
   /**
     * Method for storing the object for the complete network
     * @param storeType type of representation of interest
     * @param net net under analysis
     * @return size of the file containing the info of the
     *         net
     */
   def serializeNet(folder : String, netName : String,
                    storeType : ValueStoreType, net : Bnet) = {
      // creates the folder for the objects of the network
      // under analysis
      val path = folder + "/" +  netName + "/"
      val netFolder = new File(path)

      // creates the folder if needed
      if(!netFolder.exists()){
         netFolder.mkdir()
      }

      // compose the name of the object
      val netFileName = path + storeType.toString + "-net"

      // serialize Bnet object
      Bnet.writeObject(netFileName, net)

      // return the size of the object
      new File(netFileName).length()
   }

   /**
     * Serializes potential objects
     * @param potential
     * @return size of the file containing the info about the
     *         potential
     */
   def serializePotential(folder : String, netName : String,
                          potential: Potential) = {
      // creates the folder for the objects of the network
      // under analysis
      val path = folder + "/" +  netName + "/"
      val netFolder = new File(path)

      // creates the folder if needed
      if(!netFolder.exists()){
         netFolder.mkdir()
      }

      // compose the name of the object
      val potentialFileName = path + potential.mainVariable.name +
         "-" + potential.store.kind

      // store the potential
      Potential.writeObject(potentialFileName, potential)

      // now return the size of the file
      new File(potentialFileName).length()
   }
}

/**
  * Class for storing the information about the potentials
  * obtained through each representation
  * @constructor creates an object of BnetInfo with data
  *              passed as argument
  * @param folder folder where objects with serialization
  *               will be stored
  * @param netName name of the net to analyze
  * @param storeType type of representation of interest
  * @param net Bnet object
  * @param serialize flag to indicate if serialization info
 *                  must be computed
  */
class BnetInfo(val folder:String, val netName: String,
               val storeType: ValueStoreType, val net : Bnet,
               val serialize : Boolean){

   /**
     * Creates a mutable map with entries formed by Variable
     * (key) and PotentialInfo (value)
     */
   val potentialsInfo = Map[Variable, PotentialInfo]()

   // serialize the net
   /**
     * Stores serialization sizde of the complete net
     */
   val serializationSize: Long =
      if(serialize) UtilAnalysis.serializeNet(folder, netName, storeType, net)
      else 0L

   // for each potential gets the required information
   net.potentials.foreach(potential => {
      // stores a new pair into ponentialsInfo
      potentialsInfo.put(potential.mainVariable,
         new PotentialInfo(folder, netName, potential, serialize))
   })
}

/**
  * Class for representing the information of interest of a given
  * potential
  * @constructor creates a new object from the information
  *              passed as argument
  * @param folder folder where objects with potential info are
  *               stored
  * @param netName name of the net of interest
  * @param potential potential of interest
  * @param serialize flag to show if serialization info must be computed
  */
class PotentialInfo(val folder:String, val netName: String,
                    val potential : Potential, val serialize : Boolean){
   /**
     * Gets the main variable of the potential
     */
   val mainVariable = potential.mainVariable

   /**
     * Gets the conditioning vars
     */
   val conditioningVars = VariableSet(potential.conditioningVars)

   /**
     * Gets info about sizes: number of indices represented, values
     * stored, number of different values
     */
   val sizes: (Long, Long, Long) = potential.store.getSize

   /**
     * estimation of memory size required for storing the potential
     * information
     */
   val memorySize = potential.getMemorySize

   // gets the size of the serialized version of the potential object
   /**
     * size of the object obtained after serializing the potential
     */
   val serializationSize = if(serialize)
                  UtilAnalysis.serializePotential(folder, netName,
                     potential)
   else 0L
}
