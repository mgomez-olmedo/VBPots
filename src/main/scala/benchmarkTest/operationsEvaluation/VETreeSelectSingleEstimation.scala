package benchmarkTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import potential.{OperatorType, ValueStoreTypes}

/**
 * Object to perform comparisons in the application of
 * VariableElimination algorithm on MIM representation
 * selecting network, extension, alternatives for combination
 * and marginalization and seed for variables selection
 */
object VETreeSelectSingleEstimation extends App {
   /**
    * propagate with an engine and a certain number of variables
    * @param engine
    * @param indexes
    */
   def propagate(engine : VariableElimination, indexes : List[Int]) = {
      indexes.foreach(index => {
         val variable = engine.bnet.variables.getVariable(index)
         engine.propagate(variable.name)
      })
   }

   // define the net name to propagate
   var netName = args(0)
   var extension= args(1)
   val seed = args(2).toInt
   val numberVariables = 10

   // read bnet info
   println("reading bnet info " + netName+ " .............")
   val bnet = Bnet(netName + "." + extension)

   // selects the variables to propagate
   // sets the number of variables according to the number
   // of variables in the net
   val finalNumberVariables: Int = if(bnet.variables.size < numberVariables)
      bnet.variables.size
   else
      numberVariables
   println("final number of variables: " + finalNumberVariables)

   // selects the indexes of variables to propagate
   val variableIndexes = Bnet.randomSelectVariableIndexes(bnet,
      finalNumberVariables, seed)

   // shows the names of the variables
   println("variables to propagate:  " + variableIndexes.
      map(bnet.variables.getVariable(_).name).mkString(" "))

   // creates a engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   var engine = new VariableElimination(bnetTree, false)

   // propagation with trees
   engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   var time1 = System.currentTimeMillis()
   propagate(engine, variableIndexes)
   var time2 = System.currentTimeMillis()
   println("tree time: " + (time2 - time1))

}
