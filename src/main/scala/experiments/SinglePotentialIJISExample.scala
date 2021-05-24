package experiments

import experiments.generation.PotentialsGeneration.generateIJISExample
import potential.ValueStoreTypes
import potential.valueBased.VDGLStore

class SinglePotentialIJISExample(percentage : Double) {
   val potential = generateIJISExample(percentage)
   print(potential)
}

object SinglePotentialIJISExample extends App{
   val typesOfInteres = List(
      ValueStoreTypes.TREE,
      ValueStoreTypes.PRUNEDTREE,
      ValueStoreTypes.VDGLIST, // VDG
      ValueStoreTypes.VDILISTIMMUT, // VDI
      ValueStoreTypes.IDPIMMUT, // IDP
      ValueStoreTypes.IDMMUT // IDM
   )

   // create object
   val example = new SinglePotentialIJISExample(0.70)
   val difValues = example.potential.store.getDifferentValues
   println("dif values: " + difValues)

   // convert into tree
   val tree = example.potential.convert(ValueStoreTypes.TREE)
   println(tree)

   // convert into pruned tree
   val prunedTree = example.potential.convert(ValueStoreTypes.PRUNEDTREE)
   println(prunedTree)

   // convert into VDG
   val vdg = example.potential.convert(ValueStoreTypes.VDGLIST)
   val grainData = vdg.store.asInstanceOf[VDGLStore].getGrainsInfo
   println("info about grains: " + grainData)
   println(vdg)

   // convert into VDI
   val vdi = example.potential.convert(ValueStoreTypes.VDILISTIMMUT)
   println(vdi)

   // convert into IPD
   val idp = example.potential.convert(ValueStoreTypes.IDPIMMUT)
   println(idp)

   // convert into IDM
   val idm = example.potential.convert(ValueStoreTypes.IDMMUT)
   println(idm)
}
