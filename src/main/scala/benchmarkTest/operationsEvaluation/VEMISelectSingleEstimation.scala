package benchmarkTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import potential.ValueStoreTypes

/**
 * Object to perform comparisons in the application of
 * VariableElimination algorithm on MIM representation
 * selecting network, extension, alternatives for combination
 * and marginalization and seed for variables selection
 */
object VEMISelectSingleEstimation extends App {
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
   val combAlternative = args(2).toInt
   val margAlternative = args(3).toInt
   val seed = args(4).toInt
   println("seed for indexes generation: " + seed)
   val numberVariables = 10

   // read bnet info
   println("reading bnet info " + netName+ " .............")
   val filename = netName + "-obj-" + ValueStoreTypes.IDMMUT.toString +
      "." + extension

   // convert the bnet to the desired representation
   val bnet = Bnet.readObject(filename)

   // selects the indexes of variables to propagate
   val variableIndexes = Bnet.randomSelectVariableIndexes(bnet,
      numberVariables, seed)

   // shows the names of the variables
   println("variables to propagate:  " + variableIndexes.
      map(bnet.variables.getVariable(_).name).mkString(" "))

   // creates a engine
   var engine = new VariableElimination(bnet, false)

   // propagation with engine

   // gets the number of alternatives for this representation
   val combAlternatives =
      bnet.potentials(0).store.combinationFunctions
   val margAlternatives =
      bnet.potentials(0).store.marginalizationFunctions

   if(combAlternative >= combAlternatives.size ||
     margAlternative >= margAlternatives.size){
      println("error in alternative selection")
      System.exit(0)
   }

   // sets the operator
   val selectedCombOperator = combAlternatives.keys.filter(
      operator => operator.id == combAlternative).toArray
   val selectedMargOperator = margAlternatives.keys.filter(
      operator => operator.id == margAlternative).toArray
   println("combination function: " + selectedCombOperator(0).toString)
   println("marginalization function: " + selectedMargOperator(0).toString)

   // set selected functions
   engine.setFunctions(selectedCombOperator(0),
      selectedMargOperator(0))

   var time1 = System.currentTimeMillis()
   propagate(engine, variableIndexes)
   var time2 = System.currentTimeMillis()
   println("tree time: " + (time2 - time1))

}
