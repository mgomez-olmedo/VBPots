package experiments.ijisExperiments.extremeCase

import experiments.generation.PotentialsGeneration.generateExtremeCasePotential
import potential.ValueStoreTypes
import potential.valueBased.VDGLStore

/**
 * Class for getting insight about potentials storage of
 * extreme case distributions. It offers a single potential
 * for analyzing its properties
 * @param percentage
 */
class ExtremeCaseAnalysis(percentage : Double) {
   val potential = generateExtremeCasePotential(percentage)
   print(potential)
}

/**
 * Companion object for performing the experiment
 */
object ExtremeCaseAnalysis extends App{
   val typesOfInteres = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLSTORE, // VDG
      ValueStoreTypes.VDILISTIMMUT, // VDI
      ValueStoreTypes.IDPISTORE, // IDP
      ValueStoreTypes.IDMMSTORE // IDM
   )

   // create object
   val percentage = 0.7
   val example = new ExtremeCaseAnalysis(percentage)
   val difValues = example.potential.store.getDifferentValues
   println("dif values: " + difValues)

   // convert into tree
   val tree = example.potential.convert(ValueStoreTypes.TREE)
   println(tree)

   // convert into pruned tree
   val prunedTree = example.potential.convert(ValueStoreTypes.PRUNEDTREE)
   println(prunedTree)

   // convert into VDG
   val vdg = example.potential.convert(ValueStoreTypes.VDGLSTORE)
   val grainData = vdg.store.asInstanceOf[VDGLStore].getGrainsInfo
   println("info about grains: " + grainData)
   println(vdg)

   // convert into VDI
   val vdi = example.potential.convert(ValueStoreTypes.VDILISTIMMUT)
   println(vdi)

   // convert into IPD
   val idp = example.potential.convert(ValueStoreTypes.IDPISTORE)
   println(idp)

   // convert into IDM
   val idm = example.potential.convert(ValueStoreTypes.IDMMSTORE)
   println(idm)

   // show summary information about all the representations
   println("\n----------------Summary of memory sizes------------------")
   println("1DA: " + example.potential.getMemorySize)
   println("PT: " + tree.getMemorySize)
   println("PPT: " + prunedTree.getMemorySize)
   println("VDG: " + vdg.getMemorySize)
   println("VDI: " + vdi.getMemorySize)
   println("IDP: " + idp.getMemorySize)
   println("IDM: " + idm.getMemorySize)
}
