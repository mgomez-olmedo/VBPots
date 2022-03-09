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
   val refSolution = refEngine.propagate("dysp")
   println("reference solution: ")
   println(refSolution)
   println("---------------------------------------------")

   // creates engines for trees and AIMStore
   val treeEngine=new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.TREE), false)
   val vdilmstore = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.VDILMSTORE), false)
   val idsmStore = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.IDSMSTORE), false)
   val idmmstore = new VariableElimination(
      Bnet.convert(bnet, ValueStoreTypes.IDMMSTORE), false)

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
            println("alternative potential--------------------------")
            println(potTree)
            println("----------------------------------------")
         }
         comparison
      }).filter(_ == false)

      // return the result of comparison (empty list or with
      // content in case of differences)
      res.isEmpty
   }

   /**
    * method for comparing the result of two propagations producing
    * a single potential
    * @param refPots potentials of the reference solution
    * @param altPots potentials of the alternative solution
    */
   def compareResults(refPot : Potential, altPot : Potential) : Boolean = {
         // now check the equality of both potential
         val comparison = (refPot == altPot)
         if(comparison == false){
            println("ref potential---------------------------")
            println(refPot)
            println("alternative potential--------------------------")
            println(altPot)
            println("----------------------------------------")
         }

      // return the result of comparison (empty list or with
      // content in case of differences)
      comparison
   }

   // test evaluation with tree
   test("comparison with trees") {
      treeEngine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      val treeSolution = treeEngine.propagate("dysp")

      // the test is ok if all the potentials are the same
      // ans res is empty
      assert(compareResults(refSolution, treeSolution))
   }

   // test evaluation with AIMStore
   test("comparison with IDMMSTORE") {
      idmmstore.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      val alternativeSolution = idmmstore.propagate("dysp")

      // the test is ok if all the potentials are the same
      // ans res is empty
      assert(compareResults(refSolution, alternativeSolution))
   }


   /*
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
   }*/
}
