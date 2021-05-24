package experiments.serializeNets

import bnet.Bnet
import potential.ValueStoreTypes

import java.util.Calendar

object ReadSerializeNets extends App{
  // folder to consider
  val folder = "./data/UAI/selected/"

  // sets net name
  val netName = "BN_125"

  // extension
  val extension = "uai"

  // create Bnet object reading from file
  println("Reading file")
  println("time: " + Calendar.getInstance().getTime())

  // convert it to the desired representations
  val representations = List(
    ValueStoreTypes.TREE,
    ValueStoreTypes.PRUNEDTREE,
    ValueStoreTypes.VDGLIST,
    ValueStoreTypes.VDILISTIMMUT,
    ValueStoreTypes.IDPIMMUT,
    ValueStoreTypes.IDSETIMMUT,
    ValueStoreTypes.IDMMUT)

  // makes conversions and store files
  representations.foreach(representation => {
    // make conversion
    println("reading serialized version for " + representation.toString)
    val filename = folder + netName + "-obj-" + representation.toString + "." + extension
    println("time: " + Calendar.getInstance().getTime())
    val bnet = Bnet.readObject(filename)

    // serialize object
    println("finisehd read at : " + Calendar.getInstance().getTime())
  })
}
