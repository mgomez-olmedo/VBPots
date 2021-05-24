package potential.tree

import base.{Variable, VariableSet}
import org.scalatest.FunSuite
import potential.{Potential, PrunedTreeStore, TreeStore, ValueStore}

/**
  * Class for testing the combination of potentials as trees
  */
class PruneTreeTest extends FunSuite{
   val variable1 = new Variable("X1", List("x11", "x12"))
   val variable2 = new Variable("X2", List("x21", "x22", "x23"))
   val variable3 = new Variable("X3", List("x31", "x32"))

   // Creates a variable set with variable1, variable2, variable3 and variable4
   val variableSet1 = new VariableSet(List(variable1, variable2, variable3))

   // define the values to use in a potential of X1(2) and X2(3), X3(5)
   val valuesPot1=Array(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
      0.5, 0.5, 0.1, 0.9, 0.5, 0.5)

   // creates both potential
   val potential1Values=PrunedTreeStore(variableSet1, valuesPot1)
   val potential1=Potential(potential1Values)

   println("Potential 1: ")
   println(potential1)

   test("Prune operation "){
      val prunedTree = potential1Values.prune
      println(prunedTree.asInstanceOf[TreeStore].printTree)
      assert(true)
   }
}