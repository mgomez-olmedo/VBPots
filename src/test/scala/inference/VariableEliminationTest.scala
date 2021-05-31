package inference

import bnet.Bnet
import org.scalatest.FunSuite
import potential.{OperatorType, Potential, ValueStoreTypes}

class VariableEliminationTest extends FunSuite{
   // define the net name to propagate
   var netName = "asia"
   var extension="net"

   // creates the engine for tables acting as reference
   // solution
   val bnet = Bnet(netName + "." + extension)
   val refEngine=new VariableElimination(bnet, false)
   refEngine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val refSolution = refEngine.propagate

   // creates engines for trees and AIMStore
   val treeEngine=new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.TREE), false)
   val aimEngine = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.IDPMSTORE), false)
   val asimEngine = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.IDSMSTORE), false)
   val mlimEngine = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.VDILMSTORE), false)

   /**
    * method for comparing the results of two propagations
    * @param refPots potentials of the reference solution
    * @param altPots potentials of the alternative solution
    */
   def compareResults(refPots : List[Potential], altPots : List[Potential]) : Boolean = {
      // now compares all the potentials
      val res = refPots.map(potential => {
         val target = potential.mainVariable
         val potTree =
            altPots.find(potential => potential.mainVariable == target).get

         // now check the equality of both potential
         val comparison = (potential == potTree)
         if(comparison == false){
            println("ref potential---------------------------")
            println(potential)
            println("tree potential--------------------------")
            println(potTree)
            println("----------------------------------------")
         }
         comparison
      }).filter(_ == false)

      // return the result of comparison (empty list or with
      // content in case of differences)
      res.isEmpty
   }

   // test evaluation with tree
   test("comparison with trees") {
      val testEngine = new VariableElimination(
         Bnet.convert(bnet, ValueStoreTypes.TREE), false)
      testEngine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      val treeSolution = testEngine.propagate

      // the test is ok if all the potentials are the same
      // ans res is empty
      assert(compareResults(refSolution, treeSolution))
   }

   // test with AIMStore and several operator types
   var combiners = List(OperatorType.DEFAULT, OperatorType.ALT1, OperatorType.ALT2,
      OperatorType.ALT3, OperatorType.ALT4)
   var marginalizers = List(OperatorType.DEFAULT, OperatorType.ALT1,
      OperatorType.ALT2)

   for(comb <- combiners; marg <- marginalizers) {
      test(s"comparison AIM (comb $comb - marg $marg)") {
         println(s"comparison AIM: (comb $comb - marg $marg)")
         aimEngine.setFunctions(comb, marg)
         val altSolution = aimEngine.propagate

         // check solutions
         assert(compareResults(refSolution, altSolution))
      }
   }

   // test with AIMStore and several operator types
   combiners = List(OperatorType.DEFAULT, OperatorType.ALT1, OperatorType.ALT2,
      OperatorType.ALT3, OperatorType.ALT4)
   marginalizers = List(OperatorType.DEFAULT, OperatorType.ALT1,
      OperatorType.ALT2)

   for(comb <- combiners; marg <- marginalizers) {
      test(s"comparison ASIM (comb $comb - marg $marg)") {
         println(s"comparison ASIM: (comb $comb - marg $marg)")
         asimEngine.setFunctions(comb, marg)
         val altSolution = asimEngine.propagate

         // check solutions
         assert(compareResults(refSolution, altSolution))
      }
   }

   // test with MLIM store (using the same set of marginalizers and
   // combiners)
   for(comb <- combiners; marg <- marginalizers) {
      test(s"comparison MLIM (comb $comb - marg $marg)") {
         println(s"comparison MLIM: (comb $comb - marg $marg)")
         mlimEngine.setFunctions(comb, marg)
         val altSolution = mlimEngine.propagate

         // check solutions
         assert(compareResults(refSolution, altSolution))
      }
   }
}
