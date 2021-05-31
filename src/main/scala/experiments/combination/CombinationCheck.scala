package experiments.combination

import potential.{OperatorType, Potential, ValueStore, ValueStoreTypes}
import utils.Util

/**
 * class for testing combination on a particular pair
 * of potentials
 */
object CombinationCheck extends App{
   /**
    * parameters defining the folder where the potentials
    * are stored
    */
   val maxCard = 1500
   val minCard = 800
   val levels = 50
   val id = 0

   // compose the path to the folder containing the objects
   // with potential values
   val folder = "./data/potentialObjects/comb/" + maxCard + "-" +
      minCard + "-" + levels + "/"

   // read potential objects
   val filename1 = folder + "pot1-" + id + ".pot"
   val filename2 = folder + "pot2-" + id + ".pot"
   println("    potential 1 name: " + filename1)
   println("    potential 2 name: " + filename2)

   // read potential and indexes
   val potential1 = Potential.readObject(filename1)
   val potential2 = Potential.readObject(filename2)

   // perform combination with tables representation
   potential1.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultTable = potential1.combine(potential2)

   // now convert potentials into trees and compare the result
   val pot1Tree = potential1.convert(ValueStoreTypes.TREE)
   val pot2Tree = potential2.convert(ValueStoreTypes.TREE)
   pot1Tree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)

   // make combination
   val resultTree = pot1Tree.combine(pot2Tree)

   // compare result values one by one
   var index = (0L until resultTable.variables.possibleValues).find(index =>
         !Util.nearEqual(resultTable.store.getValue(index),
                         resultTree.store.getValue(index))).getOrElse(-1L)

   println("indexes with different values (tree): " + index)
   if(index != -1){
      println("value in table: " + resultTable.store.getValue(index))
      println("value in tree: " + resultTree.store.getValue(index))
   }

   // now convert to AAIM store
   val pot1AAIM = potential1.convert(ValueStoreTypes.IDPMSTORE)
   val pot2AAIM = potential2.convert(ValueStoreTypes.IDPMSTORE)

   // set functions
   pot1AAIM.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   val resultAAIM = pot1AAIM.combine(pot2AAIM)

   // compare result with respect to tables
   index = (0L until resultTable.variables.possibleValues).find(index =>
      !Util.nearEqual(resultTable.store.getValue(index),
         resultAAIM.store.getValue(index))).getOrElse(-1)

   println("indexes with different values (aaim): " + index)
   if(index != -1){
      println("value in table: " + resultTable.store.getValue(index))
      println("value in tree: " + resultTree.store.getValue(index))
   }
}
