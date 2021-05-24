package benchmarkTest.operationsEvaluation

import bnet.Bnet
import inference.VariableElimination
import org.scalameter._
import potential.{OperatorType, ValueStoreTypes}

/**
  * Object to perform comparisons in the application of
  * VariableElimination algorithm on a given network and
  * using different representations for potentials
  */
object VETableTreeMSIMBenchmark extends App {

   // defines the configuration for benchmarking
   val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 40,
      Key.exec.benchRuns -> 25,
      Key.verbose -> false
   ) withWarmer new Warmer.Default

   // define the net name to propagate
   // define the net name to propagate
   var netName = "insurance"
   var extension="net"

   // creates the engine for tables
   val bnet = Bnet(netName + "." + extension)
   val engineTables=new VariableElimination(bnet,false)

   // creates a engine for tree
   val bnetTree = Bnet.convert(bnet, ValueStoreTypes.TREE)
   val engineTree = new VariableElimination(bnetTree, false)

   // creates a engine for Map indices
   val bnetMapIndices = Bnet.convert(bnet, ValueStoreTypes.VDISETMUT)
   val engineMSIMutable = new VariableElimination(bnetMapIndices, false)

   // general test: propagation with CPT, PTs and msimut
   // starts with CPT representation
   val cptTime = standardConfig measure {
      engineTables.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineTables.propagate
   }

   val treeTime = standardConfig measure {
      engineTree.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineTree.propagate
   }

   println("...............starting work with MSIM - def - def ..............")
   val msimutTimeDefDef = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.DEFAULT, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt1 - def ..............")
   val msimutTimeAlt1Def = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT1, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt1 - alt1 ..............")
   val msimutTimeAlt1Alt1 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT1)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt1 - alt2 ..............")
   val msimutTimeAlt1Alt2 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT1, OperatorType.ALT2)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt2 - def ..............")
   val msimutTimeAlt2Def = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT2, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt2 - alt1 ..............")
   val msimutTimeAlt2Alt1 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT1)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt2 - alt2 ..............")
   val msimutTimeAlt2Alt2 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT2, OperatorType.ALT2)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt3 - def ..............")
   val msimutTimeAlt3Def = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT3, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt3 - alt1 ..............")
   val msimutTimeAlt3Alt1 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT1)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt3 - alt2 ..............")
   val msimutTimeAlt3Alt2 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT3, OperatorType.ALT2)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt4 - def ..............")
   val msimutTimeAlt4Def = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT4, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt4 - alt1 ..............")
   val msimutTimeAlt4Alt1 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT1)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt4 - alt2 ..............")
   val msimutTimeAlt4Alt2 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT4, OperatorType.ALT2)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt5 - def ..............")
   val msimutTimeAlt5Def = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT5, OperatorType.DEFAULT)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt5 - alt1 ..............")
   val msimutTimeAlt5Alt1 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT5, OperatorType.ALT1)
      engineMSIMutable.propagate
   }

   println("...............starting work with MSIM - alt5 - alt2 ..............")
   val msimutTimeAlt5Alt2 = standardConfig measure {
      engineMSIMutable.setFunctions(OperatorType.ALT5, OperatorType.ALT2)
      engineMSIMutable.propagate
   }

   // finally show ratios respect to engine tables
   println("tree ratio: " + (treeTime.value/cptTime.value))
   println("msimut ratio (def - def): " + (msimutTimeDefDef.value/cptTime.value))
   println("msimut ratio (alt1 - def): " + (msimutTimeAlt1Def.value/cptTime.value))
   println("msimut ratio (alt1 - alt1): " + (msimutTimeAlt1Alt1.value/cptTime.value))
   println("msimut ratio (alt1 - alt2): " + (msimutTimeAlt1Alt2.value/cptTime.value))
   println("msimut ratio (alt2 - def): " + (msimutTimeAlt2Def.value/cptTime.value))
   println("msimut ratio (alt2 - alt1): " + (msimutTimeAlt2Alt1.value/cptTime.value))
   println("msimut ratio (alt2 - alt2): " + (msimutTimeAlt2Alt2.value/cptTime.value))
   println("msimut ratio (alt3 - def): " + (msimutTimeAlt3Def.value/cptTime.value))
   println("msimut ratio (alt3 - alt1): " + (msimutTimeAlt3Alt1.value/cptTime.value))
   println("msimut ratio (alt3 - alt2): " + (msimutTimeAlt3Alt2.value/cptTime.value))
   println("msimut ratio (alt4 - def): " + (msimutTimeAlt4Def.value/cptTime.value))
   println("msimut ratio (alt4 - alt1): " + (msimutTimeAlt4Alt1.value/cptTime.value))
   println("msimut ratio (alt4 - alt2): " + (msimutTimeAlt4Alt2.value/cptTime.value))
   println("msimut ratio (alt5 - def): " + (msimutTimeAlt5Def.value/cptTime.value))
   println("msimut ratio (alt5 - alt1): " + (msimutTimeAlt5Alt1.value/cptTime.value))
   println("msimut ratio (alt5 - alt2): " + (msimutTimeAlt5Alt2.value/cptTime.value))
}
