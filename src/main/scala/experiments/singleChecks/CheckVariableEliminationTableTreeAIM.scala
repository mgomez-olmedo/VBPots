package experiments.singleChecks

import bnet.Bnet
import inference.VariableElimination
import potential.{OperatorType, ValueStoreTypes}

/**
 * Compares results of performing propagations with table,
 * tree and AIM
 */
object CheckVariableEliminationTableTreeAIM extends App{
   // define the net name to propagate
   var netName = "alarm"
   var extension="net"

   // creates the Bnet
   val bnet:Bnet = Bnet(netName + "." + extension)

   // creates the engine for tables
   val engineTables=new VariableElimination(bnet, false)

   // creates engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   val engineTree = new VariableElimination(bnetTree, false)

   // creates engine for Array Indices
   val bnetAI = Bnet.convert(bnet, ValueStoreTypes.IDPMUT)
   val engineAIMutable = new VariableElimination(bnetAI, false)

   // propagates with tables and show results
   engineTables.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultTA = engineTables.propagate
   println(resultTA)

   // propagates with trees and show results
   engineTree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultTR = engineTree.propagate
   println(resultTR)

   engineAIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT3)
   val resultAIM = engineAIMutable.propagate
   println(resultAIM)
}
