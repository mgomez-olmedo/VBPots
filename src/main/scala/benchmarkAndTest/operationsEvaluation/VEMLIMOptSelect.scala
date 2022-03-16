package benchmarkAndTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import org.scalameter.{Key, Warmer, config}
import potential.ValueStoreTypes

/**
 * Evaluates Variable Elimination algorithm with MLIM representation
 * and selecting network, extension and alternatives for combination
 * and marginalization operations
 */
object VEMLIMOptSelect extends App{
   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 30,
      Key.verbose -> false,
      Key.exec.independentSamples -> 1
   ) withWarmer new Warmer.Default

   /**
    * propagate with an engine and a certain number of variables
    * @param engine engine for ve propagation
    * @param indexes indexes if target variables
    */
   def propagate(engine : VariableElimination, indexes : List[Int]): Unit = {
      indexes.foreach(index => {
         val variable = engine.bnet.variables.getVariable(index)
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
   val variableIndexes = Bnet.randomSelectVariableIndexes(bnet, finalNumberVariables, 0L)

   // shows the names of the variables
   println("variables to propagate:  " + variableIndexes.
     map(bnet.variables.getVariable(_).name).mkString(" "))

   // creates a engine for tree
   val convertedBnet = Bnet.convert(bnet, ValueStoreTypes.VDILMSTORE)
   val engine = new VariableElimination(convertedBnet, false)

   // gets the number of alternatives for this representation
   val combAlternatives =
      convertedBnet.potentials.head.store.combinationFunctions
   val margAlternatives =
      convertedBnet.potentials.head.store.marginalizationFunctions

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

   // measures computation time
   val time = standardConfig measure {
      propagate(engine, variableIndexes)
   }

   println("Execution time: " + time)
}
