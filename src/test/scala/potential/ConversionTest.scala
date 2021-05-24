package potential;

import bnet.Bnet
import org.scalatest.FunSuite
import utils.Util

class ConversionTest extends FunSuite {
   // folder used for storing the result of the analysis
   val folder: String = "./analysis"

   // sets the name of the net to use for testing
   val netName = "asia"

   // bnet created from the fileName passed as argument
   val bnet: Bnet = Bnet(netName + ".net")

   // make a test for each conversion (without pruning)
   test("tree conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.TREE)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparison. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // make a test for each conversion (without pruning)
   test("pruned tree conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.PRUNEDTREE)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test grain set conversion representation
   test("set of grain conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDGSET)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test grain set conversion representation
   test("set of grain conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDGSET)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test grain list conversion representation
   test("list of grain conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDGLIST)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test grain list conversion representation
   test("list of grain conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDGLIST)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("list of indices (immutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDILISTIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("list of indices (immutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDILISTIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("list of indices (mutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDILISTMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("list of indices (mutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.VDILISTMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("array of indices (immutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDPIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("array of indices (immutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDPIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("array of indices (mutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDPMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("array of indices (mutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDPMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("set of indices (immutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDSETIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check if resultPot is null
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("set of indices (immutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDSETIMMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check if resultPot is null
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("set of indices (mutable) conversion "){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDSETMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }

   // test set of indices conversion representation
   test("set of indices (mutable) conversion 0D"){
      val alternativeBnet = Bnet.convert(bnet, ValueStoreTypes.IDSETMUT)

      // gets the values for all the values of all potentials
      val resultPot = bnet.potentials.par.find(potential => {
         // check potential values
         val mainVar = potential.mainVariable
         val alternativePot = alternativeBnet.getPotentialForVariable(mainVar)
         println("    testing for potential of " + mainVar.name)

         // compare potentials using equals auxiliar method. Return the
         // negation of the comparision. Therefore, resultPot will be
         // the potential with differences or null if both are similar
         !Util.equals(potential, alternativePot)
      }).getOrElse(null)

      // for debug purposes, shows the potentials with differences
      if(resultPot != null){
         println("Difference comparing potentials: ")
         println(resultPot)
         println("-------------------------------------------------")
         println(alternativeBnet.getPotentialForVariable(resultPot.mainVariable))
         println("-------------------------------------------------")
      }

      // check there is no potential with different values
      assert(resultPot == null)
   }
}
