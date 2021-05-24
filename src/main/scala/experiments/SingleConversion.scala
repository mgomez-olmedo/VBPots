package experiments

import bnet.Bnet
import potential.ValueStoreTypes

/**
  * Object extending from App for performing a single
  * conversion of a net
  */
object SingleConversion extends App{
   /**
     * folder used for storing the result of the analysis
     */
   val folder: String = "./analysis"

   /**
    * name of the net to use for testing
    */
   val netName = "andes"

   /**
    * bnet created from the fileName passed as argument
    */
   val bnet: Bnet = Bnet(netName + ".net")

   /**
     * alternative representation
     */
   val alternativeBnet = Bnet.convert(bnet,
      ValueStoreTypes.VDISETIMMUT)

   /**
     * get potentials for variables
     */
   bnet.potentials.foreach(potential => {
      // check potential values
      val mainVar = potential.mainVariable
      val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
      println("    testing for potential of " + mainVar.name)
      println("base potential (CPT): ")
      println(potential)
      println("alternative representation (MAPSETINDICESIMMUT): ")
      println(alternativePot)
   })
}
