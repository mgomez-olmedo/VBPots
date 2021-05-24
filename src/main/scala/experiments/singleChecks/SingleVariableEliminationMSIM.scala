package experiments.singleChecks

import bnet.Bnet
import inference.VariableElimination
import potential.ValueStoreTypes

import scala.util.Random

object SingleVariableEliminationMSIM extends App{
   /**
    * prepares the indexes of the variables to propagate
    * @param bnet
    * @param counter
    * @return
    */
   def prepareVariableIndexes(bnet: Bnet, counter : Int ) : List[Int] = {
      // for each pair select two potentials
      Random.setSeed(0L)

      // shuffle potential variables
      val indexes = Random.shuffle((0 until bnet.variables.size).toList)

      // takes first variables (as indicated by counter)
      val selectedIndexes: List[Int] = indexes.take(counter.toInt)

      // return the list of selected indexes
      selectedIndexes
   }

   /**
    * propagate with an engine and a certain number of variables
    * @param engine
    * @param indexes
    */
   def propagate(engine : VariableElimination, indexes : List[Int]) = {
      indexes.foreach(index => {
         val variable = engine.bnet.variables.getVariable(index)
         println("------------ " + variable.name + "----------------")
         engine.propagate(variable.name)
      })
   }

   // define the net name to propagate
   // define the net name to propagate
   var netName = args(0)
   var extension= args(1)
   val combAlternative = args(2).toInt
   val margAlternative = args(3).toInt
   val numberVariables = 10

   // creates the engine for tables
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
   val variableIndexes = prepareVariableIndexes(bnet, finalNumberVariables)

   // shows the names of the variables
   println("variables to propagate:  " + variableIndexes.
     map(bnet.variables.getVariable(_).name).mkString(" "))

   // creates a engine for tree
   val convertedBnet = Bnet.convert(bnet, ValueStoreTypes.VDISETMUT)
   val engine = new VariableElimination(convertedBnet, false)

   // gets the number of alternatives for this representation
   val combAlternatives =
      convertedBnet.potentials(0).store.combinationFunctions
   val margAlternatives =
      convertedBnet.potentials(0).store.marginalizationFunctions

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

   engine.setFunctions(selectedCombOperator(0),
      selectedMargOperator(0))
   propagate(engine, variableIndexes)
}
