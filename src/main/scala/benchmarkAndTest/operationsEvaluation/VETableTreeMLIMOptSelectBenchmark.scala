package benchmarkAndTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import org.scalameter._
import potential.{OperatorType, ValueStoreTypes}

/**
 * Object to perform comparisons in the application of
 * VariableElimination algorithm on a given network and
 * using MLIM representation. The execution allows to
 * select net, extension, alternatives for combination
 * and marginalization and seed for variables selection
 */
object VETableTreeMLIMOptSelectBenchmark extends App {
   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 25,
      Key.verbose -> false
   ) withWarmer new Warmer.Default

   /**
    * propagate with an engine and a certain number of variables
    * @param engine engine for variable elimination propagation
    * @param indexes indexes of target variables
    */
   def propagate(engine: VariableElimination, indexes: List[Int]) = {
      indexes.foreach(index => {
         val variable = engine.bnet.variables.getVariable(index)
         engine.propagate(variable.name)
      })
   }

   // define the net name to propagate
   // define the net name to propagate
   // define the net name to propagate
   // define the net name to propagate
   var netName = args(0)
   var extension = args(1)
   val combAlternative = args(2).toInt
   val margAlternative = args(3).toInt
   val seed = args(4).toInt
   val numberVariables = 10

   // read bnet info
   val bnet = Bnet(netName + "." + extension)

   // selects the variables to propagate
   // sets the number of variables according to the number
   // of variables in the net
   val finalNumberVariables: Int = if (bnet.variables.size < numberVariables)
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

   // creates engine for tables
   var engine = new VariableElimination(bnet, false)

   // propagation with CPTs
   engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   var time = standardConfig measure {
      propagate(engine, variableIndexes)
   }
   println("table time: " + time.value)

   // creates a engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   engine = new VariableElimination(bnetTree, false)

   // propagation with trees
   engine.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
   time = standardConfig measure {
      propagate(engine, variableIndexes)
   }
   println("tree time: " + time.value)

   // creates a engine for Map with list of indices
   val bnetMapIndices = Bnet.convert(bnet, ValueStoreTypes.VDILMSTORE)
   engine = new VariableElimination(bnetMapIndices, false)

   // gets the number of alternatives for this representation
   val combAlternatives =
      bnetMapIndices.potentials.head.store.combinationFunctions
   val margAlternatives =
      bnetMapIndices.potentials.head.store.marginalizationFunctions

   if (combAlternative >= combAlternatives.size ||
      margAlternative >= margAlternatives.size) {
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

   // measure computation time
   time = standardConfig measure {
      propagate(engine, variableIndexes)
   }
   println("MLI: " + time)
}
